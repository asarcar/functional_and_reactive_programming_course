/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor.{ Props, ActorRef, ActorSystem }
import org.scalatest.{ BeforeAndAfterAll, FlatSpec }
import akka.testkit.{ TestProbe, ImplicitSender, TestKit }
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random
import scala.concurrent.duration._
import org.scalatest.FunSuite

/*
 * Next 3 lines needed if assignment tests are run in Eclipse
 */
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
@RunWith(classOf[JUnitRunner])
class BinaryTreeSuite(_system: ActorSystem) extends TestKit(_system)
    with FunSuite with ShouldMatchers with BeforeAndAfterAll
    with ImplicitSender
{
  /*
   * Alternate constructor delegates to primary constructor.
   * BinaryTreeSuite() will default to BinaryTreeSuite(ActorSystem("PostponeSpec")
   */
  def this() = this(ActorSystem("PostponeSpec"))

  override def afterAll: Unit = system.shutdown()

  import actorbintree.BinaryTreeSet._

  def receiveN(requester: TestProbe, ops: Seq[Operation],
    expectedReplies: Seq[OperationReply],
    gcCalls: Seq[Int] = Seq()): Unit =
    within(5.seconds) {
      val repliesUnsorted = for (i <- 1 to ops.size) yield try {
        requester.expectMsgType[OperationReply]
      } catch {
        case ex: Throwable if ops.size > 10 => {
          fail(s"failure to receive confirmation $i/${ops.size}", ex)
        }
        case ex: Throwable                  => {
          fail(s"failure to receive confirmation $i/${ops.size}\nRequests:" +
            ops.mkString("\n    ", "\n     ", ""), ex)
        }
      }
      val replies = repliesUnsorted.sortBy(_.id)
      if (replies != expectedReplies) {
        val pairs = (replies zip expectedReplies).zipWithIndex.
          filter (x => x._1._1 != x._1._2)
        val num_disp = pairs.head._2 + 1 + 14
        fail("Operations:\n" + ops.take(num_disp).mkString("\n") +
          "\nReplies:\n" + replies.take(num_disp).mkString("\n") +
          "\nGC-Calls: " + gcCalls.takeWhile(_ <= num_disp).mkString(" ") +
          "\n----------------\n" +
          "unexpected replies:" +
          pairs.map(x =>
            s"at index ${x._2}: got ${x._1._1}, expected ${x._1._2}").
          mkString("\n    ", "\n    ", ""))
      }
    }

  def verify(probe: TestProbe, ops: Seq[Operation],
    expected: Seq[OperationReply],
    gcCalls: Seq[Int] = Seq()): Unit = {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    var idx = 0
    ops foreach { op =>
      topNode ! op
      if (gcCalls.contains(idx)) topNode ! GC
      idx += 1
    }

    receiveN(probe, ops, expected, gcCalls)
  }

  test("proper inserts, lookups, and remove") {
    val requester = TestProbe()
    val requesterRef = requester.ref
    val ops = List(
      Contains(requesterRef, id = 1,   1),
      Insert(requesterRef,   id = 2,   1),
      Contains(requesterRef, id = 3,   1),
      Insert(requesterRef,   id = 4,  -3),
      Insert(requesterRef,   id = 5,   4),
      Insert(requesterRef,   id = 6,   3),
      Insert(requesterRef,   id = 7,   2),
      Contains(requesterRef, id = 8,   2),
      Contains(requesterRef, id = 9,  -2),
      Contains(requesterRef, id = 10,  3),
      Remove(requesterRef,   id = 11,  3),
      Contains(requesterRef, id = 12,  3)
    )
    val expectedReplies = List(
      ContainsResult(   id = 1,    false),
      OperationFinished(id = 2          ),
      ContainsResult(   id = 3,     true),
      OperationFinished(id = 4          ),
      OperationFinished(id = 5          ),
      OperationFinished(id = 6          ),
      OperationFinished(id = 7          ),
      ContainsResult(   id = 8,     true),
      ContainsResult(   id = 9,    false),
      ContainsResult(   id = 10,    true),
      OperationFinished(id = 11         ),
      ContainsResult(   id = 12,   false)
    )

    verify(requester, ops, expectedReplies)
  }

  test("GC, Insert, Query") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! GC

    topNode ! Insert(testActor, id = 1, 1)
    expectMsg(OperationFinished(id = 1))

    topNode ! Contains(testActor, id = 2, 1)
    expectMsg(ContainsResult(id = 2, true))
  }

  test("Insert, Remove, GC, Query") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Insert(testActor, id = 1, 1)
    expectMsg(OperationFinished(id = 1))
    topNode ! Remove(testActor, id = 2, 1)
    expectMsg(OperationFinished(id = 2))
    topNode ! GC
    topNode ! Contains(testActor, id = 3, 1)
    expectMsg(ContainsResult(id = 3, false))
  }

  test("instruction example 1") {
    val requester = TestProbe()
    val requesterRef = requester.ref
    val ops = List(
      Insert(requesterRef, id=100, 1),
      Contains(requesterRef, id=50, 2),
      Remove(requesterRef, id=10, 1),
      Insert(requesterRef, id=20, 2),
      Contains(requesterRef, id=80, 1),
      Contains(requesterRef, id=70, 2)
      )
   
    val expectedReplies = List(
      OperationFinished(id=10),
      OperationFinished(id=20),
      ContainsResult(id=50, false),
      ContainsResult(id=70, true),
      ContainsResult(id=80, false),
      OperationFinished(id=100)     
      )

    verify(requester, ops, expectedReplies)
  }

  test("instruction example 2") {
    val requester = TestProbe()
    val requesterRef = requester.ref
    val ops = List(
      Contains(requesterRef,0,17),
      Insert(requesterRef,1,93),
      Remove(requesterRef,2,43),
      Remove(requesterRef,3,31),
      Insert(requesterRef,4,68),
      Remove(requesterRef,5,45),
      Insert(requesterRef,6,27),
      Remove(requesterRef,7,35),
      Contains(requesterRef,8,88),
      Remove(requesterRef,9,57),
      Remove(requesterRef,10,0),
      Insert(requesterRef,11,16),
      Insert(requesterRef,12,19),
      Insert(requesterRef,13,41),
      Remove(requesterRef,14,91),
      Contains(requesterRef,15,83)
    )
    val expectedReplies = List(
      ContainsResult(0,false),
      OperationFinished(1),
      OperationFinished(2),
      OperationFinished(3),
      OperationFinished(4),
      OperationFinished(5),
      OperationFinished(6),
      OperationFinished(7),
      ContainsResult(8,false),
      OperationFinished(9),
      OperationFinished(10),
      OperationFinished(11),
      OperationFinished(12),
      OperationFinished(13),
      OperationFinished(14),
      ContainsResult(15,false)
    )
    verify(requester, ops, expectedReplies)
  }
  test("instruction example 3") {
    val requester = TestProbe()
    val requesterRef = requester.ref
    val ops = List(
      Insert(requesterRef, 0, 92),
      Insert(requesterRef, 1, 56),
      Insert(requesterRef, 2, 98),
      Contains(requesterRef, 3, 8)
    )
    val expectedReplies = List(
      OperationFinished(0),
      OperationFinished(1),
      OperationFinished(2),
      ContainsResult(3,false)
    )
    val gcCalls = Seq(0)
    verify(requester, ops, expectedReplies, gcCalls)
  }

  test("behave identically to built-in set (includes GC)") {
    val rnd = new Random()
    def randomOperations(requester: ActorRef, count: Int): Seq[Operation] = {
      def randomElement: Int = rnd.nextInt(100)
      def randomOperation(requester: ActorRef, id: Int): Operation = rnd.nextInt(4) match {
        case 0 => Insert(requester, id, randomElement)
        case 1 => Insert(requester, id, randomElement)
        case 2 => Contains(requester, id, randomElement)
        case 3 => Remove(requester, id, randomElement)
      }

      for (seq <- 0 until count) yield randomOperation(requester, seq)
    }

    def referenceReplies(operations: Seq[Operation]): Seq[OperationReply] = {
      var referenceSet = Set.empty[Int]
      def replyFor(op: Operation): OperationReply = op match {
        case Insert(_, seq, elem) =>
          referenceSet = referenceSet + elem
          OperationFinished(seq)
        case Remove(_, seq, elem) =>
          referenceSet = referenceSet - elem
          OperationFinished(seq)
        case Contains(_, seq, elem) =>
          ContainsResult(seq, referenceSet(elem))
      }

      for (op <- operations) yield replyFor(op)
    }

    val requester = TestProbe()
    val topNode = system.actorOf(Props[BinaryTreeSet])
    val count = 1000

    val ops = randomOperations(requester.ref, count)
    val expectedReplies = referenceReplies(ops)

    var gcCalls: Seq[Int] = Seq()
    var idx:Int = 0
    ops foreach { op =>
      topNode ! op
      if (rnd.nextDouble() < 0.1) {
        gcCalls = gcCalls :+ idx
        topNode ! GC
      }
      idx += 1
    }
    receiveN(requester, ops, expectedReplies, gcCalls)
  }
}
