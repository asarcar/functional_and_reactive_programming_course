/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor with ActorLogging {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef =
    context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root:ActorRef = createRoot

  var pendingQueue: Queue[Operation] = Queue()

  def receive = normal

  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case msg: Operation => root ! msg // Insert, Contains, or Remove
    case GC => {
      log.debug("GC: self {} sender {}", self.toString, sender.toString)
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
    case x => assert(false, s"BinaryTreeSet should not rcv msg ${x.toString}")
  }

  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    /*
     * Queue up all other messages to pendingQueue so that they
     * are handled once we revert to "normal" receive case
     */
    case q: Operation => pendingQueue = pendingQueue enqueue q
    case GC => // ignore new GC msgs: Garbage Collection taking place
    case CopyFinished => {
      log.debug("CopyFinished: self {} child {}", self.toString, sender.toString)
      pendingQueue foreach (newRoot ! _)
      pendingQueue = Queue()
      root = newRoot
      context.become(normal)
    }
    case x => assert(false, s"BinaryTreeSet should not rcv msg ${x.toString}")
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) =
    Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean)
    extends Actor with ActorLogging {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, e) => {
      if (e == elem) {
        removed = false
        requester ! OperationFinished(id)
      } else {
        val pos: Position = if (e < elem) Left else {assert(e > elem); Right}
        if (subtrees.isDefinedAt(pos)) subtrees(pos) ! Insert(requester, id, e)
        else {
          subtrees += pos -> context.actorOf(BinaryTreeNode.props(e, false))
          requester ! OperationFinished(id)
        }
      }
    }
    case Contains(requester, id, e) => {
      if (e == elem) requester ! ContainsResult(id, !removed)
      else {
        val pos: Position = if (e < elem) Left else {assert(e > elem); Right}
        if (subtrees.isDefinedAt(pos)) subtrees(pos) ! Contains(requester, id, e)
        else requester ! ContainsResult(id, false)
      }
    }
    case Remove(requester, id, e) => {
      if (e == elem) {
        removed = true
        requester ! OperationFinished(id)
      } else {
        val pos: Position = if (e < elem) Left else {assert(e > elem); Right}
        if (subtrees.isDefinedAt(pos)) subtrees(pos) ! Remove(requester, id, e)
        else requester ! OperationFinished(id)
      }
    }
    case CopyTo(treeNode) => {
      val children = subtrees.values.toSet
      log.debug("CopyTo: self {} removed {} TreeNode {}: SubTrees {}",
        self.toString, removed, treeNode.toString, children.toString)

      /*
       * + If removed node then we can assume the node need not be added
       * + Send CopyTo request to children of this node
       *   Use elem as the proxy for the ID of the msg going from this node
       * + Evalutate termination of Actor or get ready to copy
       */
      if (!removed) treeNode ! Insert(self, elem, elem)
      children.foreach(_ ! CopyTo(treeNode))
      evalTerminateActorOrRegisterCopying(children, removed)
    }
  }

  def evalTerminateActorOrRegisterCopying(children: Set[ActorRef],
    insertConfirmed: Boolean): Unit = {
    context.become(copying(children, insertConfirmed))
    if ((children.size == 0) && insertConfirmed) {
      log.debug("Actor Stopping: insert confirmed & all children confirmed copy")
      context.parent ! CopyFinished
      context.stop(self)
    }
  }

  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree
    * has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    /*
     * Send CopyFinished to parent only when all activity is complete:
     * 1. Node insert is confirmed
     * 2. Children Copy completed
     * This ensures that recursively the parent is assured that all activity in
     * children is complete when it received CopyFinished message.
     * So when the TreeSet receives CopyFinished message from root it can
     * open up the floodgates for the "usual" operation commands
     */
    case CopyFinished => {
      log.debug("CopyFinished: self {} child {}", self.toString, sender.toString)
      evalTerminateActorOrRegisterCopying(expected - sender, insertConfirmed)
    }
    case OperationFinished(elem) => {
      log.debug("OperationFinished: self {} sender {}", self.toString, sender.toString)
      evalTerminateActorOrRegisterCopying(expected, true)
    }
    case x => assert(false, s"Msg ${x.toString} not expected in copying stage")
  }
}
