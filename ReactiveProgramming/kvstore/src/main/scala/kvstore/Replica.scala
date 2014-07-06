package kvstore

import akka.actor._
import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import akka.actor.AllForOneStrategy
import akka.actor.PoisonPill
import akka.actor.SupervisorStrategy._
import akka.actor.Terminated
import akka.pattern.{ ask, pipe }
import akka.util.Timeout
import scala.annotation.tailrec
import scala.language.postfixOps
import scala.collection.immutable.Queue

import scala.concurrent.duration._
// import scala.concurrent.ExecutionContext.Implicits.global
// import scala.concurrent.Future
// import scala.util.Success
// import scala.util.Failure

// Use the system's dispatcher as ExecutionContext
// import system.dispatcher

import kvstore.Arbiter._
import kvstore.Persistence._

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  /**
    * A successful Insert or Remove results in a reply to the
    * client in the form of an OperationAck(id) message where
    * the id field matches the corresponding id field of the
    * operation that has been acknowledged.
    * A failed Insert or Remove command results in an
    * OperationFailed(id) reply. A failure is defined as the
    * inability to confirm the operation within 1 second.
    */
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  /**
    * A Get operation results in a GetResult(key, valueOption, id)
    * message where the id field matches the value in the id
    * field of the corresponding Get message. The valueOption
    * field should contain None if the key is not present in
    * the replica or Some(value) if a value is currently
    * assigned to the given key in that replica.
    */
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  sealed trait TimeOut
  case class XactTimeOut(id: Long, client: ActorRef) extends TimeOut
  case class PersistRetryTimeOut(id: Long, persistMsg: Persist) extends TimeOut

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props)
    extends Actor with ActorLogging {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /**
   * The contents of this actor is just a suggestion,
   * you can implement it in any way you like.
   */
  var kv = Map.empty[String, String]

  /**
    * a map from secondary replicas to replicators
    */
  var secondaries = Map.empty[ActorRef, ActorRef]
  /**
    * the current set of replicators
    */
  var replicators = Set.empty[ActorRef]

  /**
    * 1. Register with arbiter
    * 2. Start Persistor: persistence actor. Life cycle of persistor
    *    is monitored by the replica. If persistor throws
    *    exception, replica restarts the persistor unless it
    *    crashes repeatedly within a short time frame.
    */
  arbiter ! Arbiter.Join
  override val supervisorStrategy =
    AllForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
      case _ => Restart
    }
  // Erlang uses: stoppingStrategy

  var persistor: ActorRef = context.actorOf(persistenceProps, "Persistor")
  context.watch(persistor)

  /**
    * Next expected sequence number for this replica from replicator
    * TODO: take care of restarts
    */
  var _nextSeqExpected = 0L
  def getNextSeqExpected = {val r = _nextSeqExpected; r}
  def incrNextSeqExpected = {_nextSeqExpected += 1}

  val numRetries = 10
  val timeOutDur: FiniteDuration = 1000 milliseconds
  val retryTimeOut: FiniteDuration = 100 milliseconds
  /**
    * 1. Get role and set replicators
    * 2. Start Persistence Actor
    * 3. For each replicator start
    */
  def receive = {
    case JoinedPrimary   => {
      context.become(leader)
    }
    case JoinedSecondary => {
      context.become(replica)
    }
    case x => assert(false, "msg $x unexpected")
  }

  /**
    * Map from query id to pending acks from
    * 1. Persistor: 
    * 2. Replicators: 
    */
  type XactId = Long
  /**
    * Client, Set of Actors where ack is pending
    */
  type AckEntry = (ActorRef, Set[ActorRef]) 
  var pendingAcks = Map.empty[XactId, AckEntry]

  def startPersistXaction(id: Long, persistMsg: Persist): Unit = {
    /**
      * 1. Schedule timeout of this transaction.
      * 2. Schedule retry of persistence
      * 3. Send the Persist and Replicator messages
      */
    val sendTo = self
    val client = sender
    context.system.scheduler.scheduleOnce(timeOutDur) {
      sendTo ! XactTimeOut(id, client)
    } (context.system.dispatcher)
    context.system.scheduler.scheduleOnce(retryTimeOut) {
      sendTo ! PersistRetryTimeOut(id, persistMsg)
    } (context.system.dispatcher)

    persistor ! persistMsg
  }

  def startOperXaction(opCmd: Operation): Unit = {
    val (id, persistMsg, repMsg): (XactId, Persist, Replicate) =
      opCmd match {
        case Insert(key, value, id) =>
          (id, Persist(key, Some(value), id),
            Replicate(key, Some(value), id))
        case Remove(key, id) =>
          (id, Persist(key, None, id),
            Replicate(key, None, id))
        case x => (-1, null, null)
      }
    val entry: AckEntry = (sender, replicators + persistor)
    pendingAcks += id -> entry
    startPersistXaction(id, persistMsg)
    replicators foreach(_ ! repMsg)
  }

  def startSnapXaction(snapCmd: Snapshot): Unit = {
    val (seq, persistMsg): (XactId, Persist) =
      snapCmd match {
        case Snapshot(key, valueOption, seq) =>
          (seq, Persist(key, valueOption, seq))
        case x => (-1, null)
      }
    val entry: AckEntry = (sender, Set(persistor))
    pendingAcks += seq -> entry
    startPersistXaction(seq, persistMsg)
  }

  def procAck(key: String, idOrSeq: XactId, operXaction: Boolean) = {
    val entry = pendingAcks.get(idOrSeq)
    if (entry == None) {
      // Xaction closed due to time out
    } else {
      // persist or replication succeeded: update pendingAcks accordingly
      val ackEntry: AckEntry = entry.get
      val client: ActorRef = ackEntry._1
      val ackPending: Set[ActorRef] = ackEntry._2 - sender
      /**
        * If no acknowledgment is pending then operation succeeded!
        * Remove the pendingAck entry and return success.
        * Otherwise update the MAP accordingly
        */
      if (ackPending.isEmpty) {
        pendingAcks -= idOrSeq
        if (operXaction) client ! OperationAck(idOrSeq)
        else {
          client ! SnapshotAck(key, idOrSeq)
          incrNextSeqExpected
        }
      } else {
        pendingAcks += idOrSeq -> (client, ackPending)
      }
    }
  }
  def procOperAck(id: XactId) = procAck("junk", id, true)
  def procSnapAck(key: String, seq: XactId) = procAck(key, seq, false)

  def procTimeOut(idOrSeq: XactId, msg: TimeOut, operXaction: Boolean) {
    val entry = pendingAcks.get(idOrSeq)
    if (entry == None) {
      // xaction was closed successfully or failed definitively
    } else {
      msg match {
        case XactTimeOut(idOrSeq, client) => {
          if (operXaction) client ! OperationFailed(idOrSeq)
          pendingAcks -= idOrSeq // stop processing this Xaction
        }
        case PersistRetryTimeOut(idOrSeq, persistMsg) => {
          val sendTo = self
          context.system.scheduler.scheduleOnce(retryTimeOut) {
            sendTo ! PersistRetryTimeOut(idOrSeq, persistMsg)
          } (context.system.dispatcher)
          persistor ! persistMsg
        }
      }
    }
  }

  def procOperTimeOut(id: XactId, msg: TimeOut) = procTimeOut(id, msg, true)
  def procSnapTimeOut(seq: XactId, msg: TimeOut) = procTimeOut(seq, msg, false)

  /**
    * Behavior for  the leader role.
    */
  val leader: Receive = {
    /**
      * Insert/Remove
      * 1. Persist locally and remotely
      *    Persist is retried before the 1 second response timeout
      *    in case persistence failed. The id used in retried
      *    Persist messages must match the one which was used
      *    in the first request for this particular update.
      * 4. If ack is received by 1 sec from all parties return OperationAck
      *    else OperationFailed
      */
    case Insert(key, value, id) => {
      startOperXaction(Insert(key, value, id))
      kv += key -> value
    }
    case Remove(key, id)        => {
      startOperXaction(Remove(key, id))
      kv -= key
    }
    case Get(key, id)           => {
      sender ! GetResult(key, kv.get(key), id)
    }
    case XactTimeOut(id, cl) =>
      procOperTimeOut(id, XactTimeOut(id, cl))
    case PersistRetryTimeOut(id, persistMsg) =>
      procOperTimeOut(id, PersistRetryTimeOut(id, persistMsg))
    case Persisted(key, id) => procOperAck(id)
    case Replicated(key, id) => procOperAck(id)
    case Replicas(replicas)     => {
      /**
        * For each replica start a replicator
        * Tie the lifetime of the replicator with that of the replica
        * Note: replicas also contains the leader: skip the self entry
        */
      var i = 0
      replicas.tail foreach {
        replica => {
          i += 1
          val replicator = context.actorOf(Replicator.props(replica),
            s"Replicator-$i")
          replicators += replicator
          secondaries += replica -> replicator
          /**
            * context.watch(replicator)
            * No need to watch replicator as it will be restarted
            * automatically by the restart logic were it to tie
            */
        }
      }
    }
    case Terminated(ref) => {
      // TODO
    }
    case x => assert(false, "msg $x unexpected in leader")
  }
  /**
    * TODO Behavior for the replica role.
    */
  val replica: Receive = {
    case Get(key, id)           => {
      sender ! GetResult(key, kv.get(key), id)
    }
    case Snapshot(key, valueOption, seq) => {
      log.info("Snapshot({}, {}, {}) requested: nextSeqExpected {}",
        key, valueOption, seq, getNextSeqExpected)
      /**
        * Send a SnapshotAck after persisting the information.
        * If persisting fails do not send any ack.
        * Respect the sequence number logic as demanded by the Protocol
        */
      if (seq == getNextSeqExpected) {
        startSnapXaction(Snapshot(key, valueOption, seq))
        if (valueOption != None) kv += key -> valueOption.get
        else kv -= key
      } else if (seq > getNextSeqExpected) {
        // ignored quietly
      } else {
        // seq < getNextSeqExpected => immediately acknoweledged and ignored
        sender ! SnapshotAck(key, seq)
      }
    }
    case XactTimeOut(seq, cl) => {
      log.info("XactTimeOut({}, {})", seq, cl)
      procSnapTimeOut(seq, XactTimeOut(seq, cl))
    }
    case PersistRetryTimeOut(seq, persistMsg) => {
      log.info("PersistRetryTimeOut({}, {})", seq, persistMsg)
      procSnapTimeOut(seq, PersistRetryTimeOut(seq, persistMsg))
    }
    case Persisted(key, seq) => {
      log.info("Persisted({}, {}): sender {}, persistor {}",
        key, seq, sender, persistor)
      procSnapAck(key, seq)
    }
    case x => {
      assert(false, "msg $x unexpected in replica")
    }  
  }

}

