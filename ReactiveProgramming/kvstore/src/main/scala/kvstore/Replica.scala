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
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure

import kvstore.Arbiter._

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
  val timeOutDur: FiniteDuration = 1 second
  val retryTimeOut: FiniteDuration = timeOutDur/numRetries
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
    * Poor Design: Lots of future
    * Future runs outside the Akka context
    * Need to restructure the code in future and design
    * using Akka
    */
  def aggrAck(client: ActorRef,
    opCmd: Operation, snapCmd: Snapshot,
    isLeader: Boolean):
      Unit = {
    val (persistMsg, repMsg, opReplySuccess, opReplyFailure, snapAck):
        (Persist, Replicate, OperationAck, OperationFailed, SnapshotAck)
    = if (isLeader) {
      opCmd match {
        case Insert(key, value, id) =>
          (Persist(key, Some(value), id),
            Replicate(key, Some(value), id),
            OperationAck(id), OperationFailed(id), null)
        case Remove(key, id) =>
          (Persist(key, None, id),
            Replicate(key, None, id),
            OperationAck(id), OperationFailed(id), null)
        case x => {
          assert(false, s"$x unexpected");
          (null, null, null, null, null)
        }
      }
    } else {
      snapCmd match {
        case Snapshot(key, optionVal, seq) =>
          (Persist(key, optionVal, seq), null, null, null,
            SnapshotAck(key, seq))
        case x => {
          assert(false, s"$x unexpected");
          (null, null, null, null, null)
        }
      }
    }

    def resolveRepl(repSoFarFut: Future[OperationReply],
      repForReplFut: Future[Replicated]): Future[OperationReply] = {
      {
        for {
          repSoFar <- repSoFarFut
          repForRepl <- repForReplFut
        } yield repSoFar
      } recover {
        case ex => opReplyFailure
      }
    }

    def retryPersist(timeLeft: FiniteDuration): Future[Persisted] = {
      implicit val timeOut:Timeout = retryTimeOut
      val persistFut = (persistor ? persistMsg) recover {
        case e: akka.pattern.AskTimeoutException
            if (retryTimeOut <= timeLeft) => {
              log.info("Msg {} failed with timeout: timeLeft {} retryTimeOut {}",
                persistMsg, timeLeft, retryTimeOut)
              retryPersist(timeLeft - retryTimeOut)
            }
        case ex => {
          log.info("Msg {} failed: with exception {}",
            persistMsg, ex)
          throw ex
        }
      }
      persistFut.mapTo[Persisted].
        recoverWith {
          case ex => Future.failed(throw new PersistenceException)
        }
    }

    /**
      * 1. Set 5 retries for the Persistence transaction and attempt persistence.
      * 2. In parallel set off replication queries to all replicators
      * 3. Reduce all these futures into one reply OperationAck or OperationFailed
      */
    val persistenceFuture = retryPersist(timeOutDur)
    if (isLeader) {
      val res: Future[OperationReply] = persistenceFuture.mapTo[Persisted].
        map(_ => opReplySuccess).recover {
          case ex => opReplyFailure
        }

      implicit val timeOut:Timeout = timeOutDur
      val setReplResp: Set[Future[Replicated]] =
        replicators.map(rep => (rep ? repMsg).mapTo[Replicated])
      /**
        * If any of these futures are not Success,
        * we must return OperationFailed
        */
      val finalReply: Future[OperationReply] = setReplResp.foldLeft(res)(resolveRepl)
      finalReply pipeTo client
    } else {
      persistenceFuture onComplete {
        /**
          * BAD Code: We are changing state of Actor (_nextSeqExpected)
          * from a different context
          */
        case Success(persistedMsg) => {incrNextSeqExpected; client ! snapAck}
        case Failure(ex) => // silently ignore: replica will timeout
      }
    }
  }

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
      aggrAck(sender, Insert(key, value, id),
        null, true)
      kv += key -> value
    }
    case Remove(key, id)        => {
      aggrAck(sender, Remove(key, id),
        null, true)
      kv -= key
    }
    case Get(key, id)           => {
      sender ! GetResult(key, kv.get(key), id)
    }
    case Replicas(replicas)     => {
      /**
        * For each replica start a replicator
        * Tie the lifetime of the replicator with that of the replica
        */
      var i = 0
      replicas foreach {
        replica => {
          i += 1
          val replicator = context.actorOf(Replicator.props(replica), "Replicator-$i")
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
        aggrAck(sender, null, Snapshot(key, valueOption, seq), false)
        if (valueOption != None) kv += key -> valueOption.get
        else kv -= key
      } else if (seq > getNextSeqExpected) {
        // ignored quietly
      } else {
        // seq < getNextSeqExpected => immediately acknoweledged and ignored
        sender ! SnapshotAck(key, seq)
      }
    }
    case x => {
      assert(false, "msg $x unexpected in replica")
    }  
  }

}

