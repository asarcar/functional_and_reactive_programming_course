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
  case class ReplicationTimeOut(id: Long, client: ActorRef) extends TimeOut
  case class PersistTimeOut(seq: Long, client: ActorRef) extends TimeOut
  case class PersistRetryTimeOut(seq: Long, persistMsg: Persist) extends TimeOut

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
  var secondaries: Map[ActorRef, ActorRef] = Map.empty
  /**
    * the current set of replicators
    */
  var replicators: Set[ActorRef] = Set.empty

  var persistor: ActorRef = context.actorOf(persistenceProps, "Persistor")
  context.watch(persistor)

  /**
    * Next expected sequence number for this replica from replicator
    * TODO: take care of restarts
    */
  var _nextSeqExpected = 0L
  def getNextSeqExpected = {val r = _nextSeqExpected; r}
  def incrNextSeqExpected = {_nextSeqExpected += 1}

  val timeOutDur: FiniteDuration = 1000 milliseconds
  val retryTimeOut: FiniteDuration = 100 milliseconds

  /**
    * A monotonically increasing identifier for any new replicas
    */
  var _replicaNum = 0
  def getNextReplicaNum = {val rN = _replicaNum; _replicaNum +=1 ; rN}

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

  def receive = {
    case JoinedPrimary   => {
      context.become(leader)
      /**
        * Kick start the KV store by self configuring
        * the leader as a replica member
        */
      procReplicas(Replicas(Set(self)))
    }
    case JoinedSecondary => {
      context.become(replica)
    }
    case x => assert(false, "msg $x unexpected")
  }

  /**
    * Map from query id to
    * 1. client actor reference AND
    * 2. reference of all Replicators from whom ack is pending
    */
  type AckEntry = (ActorRef, Set[ActorRef]) 
  var pendingReplicationAcks = Map.empty[Long, AckEntry]
  /**
    * Map from sequence to
    * client replication that initiated persistence request
    */
  var pendingPersistenceAck = Map.empty[Long, ActorRef]

  /**
    * Map of various timers so that outstanding ones may be cancelled
    * 1. ReplicationTimers: id -> cancellable
    * 2. PersistRetryTimers: seq -> cancellable
    * 3. PersistTimer: seq -> cancellable
    */
  var cancelReplicationTimers: Map[Long, Cancellable] = Map.empty 
  var cancelPersistTimers: Map[Long, Cancellable] = Map.empty
  var cancelPersistRetryTimers: Map[Long, Cancellable] = Map.empty

  override def postStop(): Unit = {
    for ((seq, can) <- cancelReplicationTimers)
      can.cancel()
    for ((seq, can) <- cancelPersistTimers)
      can.cancel()
    for ((seq, can) <- cancelPersistRetryTimers)
      can.cancel()
  }

  def startReplicationTimer(id: Long): Unit = {
    /**
      * Schedule timeout of this transaction.
      */
    val sendTo = self
    val client = sender
    cancelReplicationTimers +=
    id -> context.system.scheduler.scheduleOnce(timeOutDur) {
      sendTo ! ReplicationTimeOut(id, client)
    } (context.system.dispatcher)
  }

  def startPersistenceTimers(seq: Long, persistMsg: Persist): Unit = {
    /**
      * Schedule eventual timeout of persist xaction
      * Schedule retry of persistence and send persist message
      */
    val sendTo = self
    val client = sender
    cancelPersistTimers +=
    seq -> context.system.scheduler.scheduleOnce(timeOutDur) {
      sendTo ! PersistTimeOut(seq, client)
    } (context.system.dispatcher)
    cancelPersistRetryTimers +=
    seq -> context.system.scheduler.scheduleOnce(retryTimeOut) {
      sendTo ! PersistRetryTimeOut(seq, persistMsg)
    } (context.system.dispatcher)

    persistor ! persistMsg
  }

  def prepareForReplication(rMsg: Replicate,
    reps: Set[ActorRef]) = {
    pendingReplicationAcks += rMsg.id -> (sender, reps)
    startReplicationTimer(rMsg.id)
    reps foreach(_ ! rMsg)
  }

  def startInsertReplication(insMsg: Insert, reps: Set[ActorRef]) = {
    log.debug("Create {} with acks needed from {}", insMsg, reps)
    prepareForReplication(
      Replicate(insMsg.key, Some(insMsg.value), insMsg.id), reps)
  }

  def initiateDBSynch(reps: Set[ActorRef]) = {
    /**
      * Generate a unique ID for the DB synch
      */
    var _repId = 1000000
    def getNextId = {val id = _repId; _repId += 1; id}

    for ((key, value) <- kv)
      startInsertReplication(Insert(key, value, getNextId), reps)
  }

  def startRemoveReplication(remMsg: Remove) =
    prepareForReplication(Replicate(remMsg.key, None, remMsg.id),
      replicators)


  def procGet(g: Get) =
    sender ! GetResult(g.key, kv.get(g.key), g.id)

  def startPersistenceXaction(s: Snapshot): Unit = {
    pendingPersistenceAck += s.seq -> sender
    startPersistenceTimers(s.seq, Persist(s.key, s.valueOption, s.seq))
  }

  def procReplicationAckFromSenders(id: Long, client: ActorRef,
    pendSet: Set[ActorRef], rcvSet: Set[ActorRef]) {
    val newPendSet: Set[ActorRef] = pendSet &~ rcvSet
    log.debug("ReplAck: id {}, pend {}, rcv {}, newSet {}",
      id, pendSet, rcvSet, newPendSet)
    /**
      * If no acknowledgment is pending then operation succeeded!
      * Remove the pendingAck entry and return success.
      * Otherwise update the MAP accordingly
      */
    if (newPendSet.isEmpty) {
      log.debug("OperationAck: id {} -> client {}", id, client)
      cancelReplicationTimers(id).cancel()

      pendingReplicationAcks -= id
      cancelReplicationTimers -= id

      client ! OperationAck(id)
    } else {
      pendingReplicationAcks += id -> (client, newPendSet)
    }
  }

  def procReplicationAck(r: Replicated) = {
    val entry = pendingReplicationAcks.get(r.id)
    if (entry == None) {
      assert(!cancelReplicationTimers.isDefinedAt(r.id),
        s"Id ${r.id}: cancel replication timer and pending ack map inconsistent")
    } else {
      // replication succeeded: update pendingAcks accordingly
      val (client, reps) = entry.get
      procReplicationAckFromSenders(r.id, client, reps, Set(sender))
    }
  }

  def procPersistenceAck(p: Persisted) = {
    val seq = p.id
    val entry = pendingPersistenceAck.get(seq)
    if (entry != None) {
      // persistence succeeded
      // cancel timers
      cancelPersistTimers(seq).cancel()
      cancelPersistRetryTimers(seq).cancel()

      // remove entry
      val client: ActorRef = entry.get
      pendingPersistenceAck -= seq
      cancelPersistTimers -= seq
      cancelPersistRetryTimers -= seq

      // send Snapshot ACK
      client ! SnapshotAck(p.key, seq)
      incrNextSeqExpected
    } else {
      assert(!cancelPersistTimers.isDefinedAt(seq),
        s"Seq $seq: cancel persist timer and pending Persist Ack map insconsistent") 
      assert(!cancelPersistRetryTimers.isDefinedAt(seq),
        s"Seq $seq: cancel persist retry timer and pending Persist Ack map insconsistent") 
    }
  }

  def procReplicationTimeOut(rTO: ReplicationTimeOut) = {
    if (pendingReplicationAcks.isDefinedAt(rTO.id)) {
      log.debug("OperationFailing: msg {}", rTO)
      cancelReplicationTimers(rTO.id).cancel()

      rTO.client ! OperationFailed(rTO.id)

      pendingReplicationAcks -= rTO.id
      cancelReplicationTimers -= rTO.id
    } else {
      assert(!cancelReplicationTimers.isDefinedAt(rTO.id),
        s"Id ${rTO.id}: cancel replication timer and pending ack map inconsistent")
    }
  }

  def procPersistTimeOut(pTO: PersistTimeOut) = {
    if (pendingPersistenceAck.isDefinedAt(pTO.seq)) {
      //cancel timers
      cancelPersistTimers(pTO.seq).cancel()
      cancelPersistRetryTimers(pTO.seq).cancel()
      // remove entry
      pendingPersistenceAck -= pTO.seq
      cancelPersistTimers -= pTO.seq
      cancelPersistRetryTimers -= pTO.seq
    } else {
      assert(!cancelPersistTimers.isDefinedAt(pTO.seq),
        s"Seq ${pTO.seq}: cancel per timer & pend Per Ack map insconsistent") 
      assert(!cancelPersistRetryTimers.isDefinedAt(pTO.seq),
        s"Seq ${pTO.seq}: cancel retry pers timer & pend Pers Ack map insconsistent") 
    }
  }

  def procPersistRetryTimeOut(pRTO: PersistRetryTimeOut) = {
    if (pendingPersistenceAck.isDefinedAt(pRTO.seq)) {
      val sendTo = self
      cancelPersistRetryTimers(pRTO.seq).cancel()
      cancelPersistRetryTimers -= pRTO.seq
      cancelPersistRetryTimers += 
      pRTO.seq -> context.system.scheduler.scheduleOnce(retryTimeOut) {
        sendTo ! PersistRetryTimeOut(pRTO.seq, pRTO.persistMsg)
      } (context.system.dispatcher)
      persistor ! pRTO.persistMsg
    } else {
      assert(!cancelPersistRetryTimers.isDefinedAt(pRTO.seq),
        s"Seq ${pRTO.seq}: cancel persist retry timer and pending Persist Ack map insconsistent") 
    }
  }

  def procSnapshot(s: Snapshot) = {
    val nextSeqExp = getNextSeqExpected
    log.debug("Snapshot({}, {}, {}) requested: nextSeqExpected {}",
      s.key, s.valueOption, s.seq, nextSeqExp)
    /**
      * Send a SnapshotAck after persisting the information.
      * If persisting fails do not send any ack.
      * Respect the sequence number logic as demanded by the Protocol
      */
    if (s.seq == nextSeqExp) {
      startPersistenceXaction(s)
      if (s.valueOption != None) kv += s.key -> s.valueOption.get
      else kv -= s.key
    } else if (s.seq > nextSeqExp) {
      // ignored quietly
    } else {
      // seq < nextSeqExp => immediately acknowledged and ignored
      sender ! SnapshotAck(s.key, s.seq)
    }
  }

  def procReplicas(r: Replicas) = {
    /**
      * Identify
      * A. Replicas that were alive and are still alive.
      * B. Old replicas that are now dead.
      * C. New replicas that have come alive.
      */
    val prevR = secondaries.keySet
    val curR = r.replicas
    val sameReplicas = curR & prevR
    val newReplicas = curR &~ prevR
    val deadReplicas = prevR &~ curR 

    log.debug("Msg Replicas {}: Same {}: New {}: Dead {}",
      r, sameReplicas, newReplicas, deadReplicas)

    /**
      * Case A: No need to do anything for these replicas
      * Handle Case B and Case C
      */
    procDeadReplicas(deadReplicas)
    procNewReplicas(newReplicas)
  }

  /**
    * Case B:
    * 1. Stop Replicator.
    * 2. Clean any pending acknowledgments to corresponding replicators
    */
  def procDeadReplicas(deadReplicas: Set[ActorRef]) = {
    var deadRepls: scala.collection.mutable.ArrayBuffer[ActorRef] =
      scala.collection.mutable.ArrayBuffer.empty

    for (ref <- deadReplicas) {
      assert(secondaries.isDefinedAt(ref),
        s"Replica $ref dead but replicator entry does not exist")
      val deadRepl = secondaries(ref)
      deadRepls += deadRepl
      context.stop(deadRepl)
    }

    val deadReplicators = deadRepls.toSet
    for ((id, (client, reps)) <- pendingReplicationAcks;
      if (!(reps & deadReplicators).isEmpty))
      procReplicationAckFromSenders(id, client, reps, deadReplicators)
  }

  /**
    * Case C:
    * For each replica
    * 1. Start a replicator and record the map from replica to replicator
    * 2. Tie the lifetime of the replicator with that of the replica
    * 3. Initiate a full DB replication to the replicator
    */
  def procNewReplicas(newReplicas: Set[ActorRef]) = {
    var newReplicators: scala.collection.mutable.ArrayBuffer[ActorRef] =
      scala.collection.mutable.ArrayBuffer.empty

    for (replica <- newReplicas) {
      assert(!replicators.contains(replica),
        "Replica $replica not a new replica: already in replicator")
      assert(!secondaries.isDefinedAt(replica),
        "Replica $replica not a new replica: already in secondaries")
      val repNum = getNextReplicaNum
      val replicator = context.actorOf(Replicator.props(replica),
        s"Replicator-$repNum")
      newReplicators += replicator
      replicators += replicator
      secondaries += replica -> replicator
      context.watch(replica)
    }

    initiateDBSynch(newReplicators.toSet)
  }

  /**
    * Behavior for  the leader role.
    */
  val leader: Receive = {
    case insMsg: Insert => startInsertReplication(insMsg, replicators)
    case remMsg: Remove => startRemoveReplication(remMsg)
    case g: Get => procGet(g)
    case s: Snapshot => procSnapshot(s)
    case rTO: ReplicationTimeOut => procReplicationTimeOut(rTO)
    case pTO: PersistTimeOut => procPersistTimeOut(pTO)
    case pRTO: PersistRetryTimeOut => procPersistRetryTimeOut(pRTO)
    case p: Persisted => procPersistenceAck(p)
    case r: Replicated => procReplicationAck(r)
    case r: Replicas => procReplicas(r)
    case Terminated(ref) => // TODO
    case x => assert(false, s"msg $x unexpected in leader")
  }
  /**
    * TODO Behavior for the replica role.
    */
  val replica: Receive = {
    case g: Get => procGet(g)
    case s: Snapshot => procSnapshot(s)
    case pTO: PersistTimeOut => procPersistTimeOut(pTO)
    case pRTO: PersistRetryTimeOut => procPersistRetryTimeOut(pRTO)
    case p: Persisted => procPersistenceAck(p)
    case Terminated(ref) => // TODO
    case x => assert(false, "msg $x unexpected in leader")
  }

}

