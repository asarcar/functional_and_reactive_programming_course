package kvstore

import akka.actor._
import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import scala.language.postfixOps

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  /**
    * The Snapshot message provides a sequence number (seq)
    * to enforce ordering between the updates. Updates for
    * a given secondary replica must be processed in contiguous
    * ascending sequence number order; this ensures that updates
    * for every single key are applied in the correct order.
    * Each Replicator uses its own number sequence starting at zero.
    *
    * When a snapshot arrives at a Replica with a sequence number
    * which is greater than the currently expected number, then that
    * snapshot must be ignored (meaning no state change and no reaction).
    * 
    * when a snapshot arrives at a Replica with a sequence number which
    * is smallernumber, then that snapshot must be ignored and immediately
    * acknowledged.
    */
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  /**
    * SnapshotAck(key, seq) is the reply sent by the secondary replica
    * to the Replicator as soon as the update is persisted locally by
    * the secondary replica. The replica might never send this reply
    * in case it is unable to persist the update.
    * 
    * The acknowledgement is sent immediately for requests whose sequence
    * number is less than the next expected number.
    *
    * The expected number is set to the greater of the previously
    * expected number the sequence number just acknowledged + 1
    */
  case class SnapshotAck(key: String, seq: Long)

  /**
    * TimerTick used for transmitting all unacknowledged Snapshots
    */
  case object TimerTick

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor with ActorLogging {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /**
    * The contents of this actor is just a suggestion, you can implement
    * it in any way you like.
    */

  /**
    * map from sequence number to pair of sender and
    * sorted array of requests. 
    */
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  /**
    * a sequence of not-yet-sent snapshots (you can disregard this
    * if not implementing batching)
    *
    * Since the replication protocol is meant to symbolize remote
    * replication you must consider the case that either a Snapshot
    * message or its corresponding SnapshotAck message is lost on
    * the way. Therefore the Replicator must make sure to
    * periodically retransmit all unacknowledged changes. For
    * grading purposes it is assumed that this happens roughly
    * every 100 milliseconds. To allow for batching (see above)
    * we will assume that a lost Snapshot message will lead to a
    * resend at most 200 milliseconds after the Replicate request
    * was received (again, the ActorSystems scheduler service is
    * considered precise enough for this purpose).
    */
  var pending = Vector.empty[Snapshot]

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  /**
    * Timer tick to retransmit all unacknowledged SnapShots every 100ms
    */
  val tickDur: FiniteDuration = 100 milliseconds
  val sendTo = self

  var cancelReplicatorTimer: Cancellable =
    context.system.scheduler.schedule(0 millisecond, tickDur){
      sendTo ! TimerTick
    }(context.system.dispatcher)

  override def postStop(): Unit = cancelReplicatorTimer.cancel()

  /**
    * Add Snapshot entries a Vector of txEntries
    * (sorted by sequence is ensures reasonable
    * performance from protocol (while sending to replicas)
    */
  def createTxEntries: Vector[Snapshot] = {
    var txArray: scala.collection.mutable.ArrayBuffer[Snapshot] =
      scala.collection.mutable.ArrayBuffer.empty
    for ((seq, (client, Replicate(key, valueOption, id))) <- acks)
      txArray += Snapshot(key, valueOption, seq)
    txArray.sortWith((e1, e2) => (e1.seq < e2.seq)).toVector
  }

  def txEntries(txVector: Vector[Snapshot]) =
    txVector foreach (replica ! _)

  def procReplReq(rep: Replicate) = {
    val seq = nextSeq // generate the next sequence
    assert(!acks.isDefinedAt(seq),
      s"$seq exists in acks map")      
    acks += seq -> (sender, rep)
  }

  def procSnapAck(snapAck: SnapshotAck) = {
    /**
      * 1. Look up the sequence in the Map.
      * 2. If the sequence does not exist, the Snapshot acknowledgement
      *    has already been processed. This must be a retx. We are done.
      * 2. Otherwise, acknowledge to sender for the corresponding entry
      */
    val seq = snapAck.seq
    log.debug("SnapAck {} received", snapAck)
    if (acks.isDefinedAt(seq)) {
      val (client, repMsg) = acks.apply(seq)
      acks -= seq // remove this entry from unacknowledged entries
      client ! Replicated(repMsg.key, repMsg.id)
    }
  }

  /**
    * TODO Behavior for the Replicator.
    */
  def receive: Receive = {
    case snapAck: SnapshotAck => procSnapAck(snapAck)
    case rep: Replicate => procReplReq(rep)
    case TimerTick => txEntries(createTxEntries)
    case x => assert(false, s"$x unexpected")
  }
}
