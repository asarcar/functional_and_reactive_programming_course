package counter

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorSystem
import akka.actor.PoisonPill
import akka.actor.Props
import akka.actor.ReceiveTimeout
import akka.actor.Terminated
import akka.testkit.ImplicitSender
import akka.testkit.TestKit
import akka.testkit.TestActor
import akka.testkit.TestProbe

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scala.util.Random

object CounterDriver {
  case class AddNum(n: Int)
  case class ValRequest
  case class ValReply(n: Int)
  case class DownRequest
  case class DownReply
}

class CounterDriver extends Actor with ActorLogging {
  import CounterDriver._
  val counter = context.actorOf(Props[Counter], "counter")
  val rnd = new Random()
  context.watch(counter)

  def receive = {
    case AddNum(n) => {
      log.debug("AddNum({}) Request...", n)
      for (i <- 0 until n)
        counter ! Counter.Incr
    }
    case ValRequest => {
      log.debug("ValRequest...")
      counter ! Counter.Get(sender)
    }
    case Counter.CurVal(x, client) => {
      log.debug("CurVal: count = {}...", x)
      client ! ValReply(x)
    }
    case DownRequest => {
      log.debug("DownRequest...")
      counter ! Counter.Down(sender)
      context.become(completeDown)
    }
      /*
       * case ReceiveTimeout => {
       *   log.debug("All work completed & terminated")
       * }
       * case Counter.DoneAck => {
       *   context.setReceiveTimeout(1 seconds)
       * }
       */
  }
  def completeDown: Receive = {
    case Counter.CurVal(x, client) => {
      log.debug("CurVal: count = {}...", x)
      client ! ValReply(x)
    }
    case Counter.DownAck(client) => {
      log.debug("All work completed & terminated")
      client ! DownReply
      // goDown
      // context.stop(self)
    }
    case Terminated(_) => {
      log.debug("Counter died suddenly")
      context.stop(self)
    }
  }

  def goDown = context.system.scheduler.
    scheduleOnce(100 + rnd.nextInt(900) millis){self ! PoisonPill}
}

object CounterMain extends App {
  println("CounterDriver Testing BEGIN...")
  println("------------------------------")

  println("Testing Counter Directly...")
  val sys = ActorSystem("CounterDriver")
  val cd = sys.actorOf(Props[CounterDriver], "CounterDriverMain")
  sys.shutdown()
  println("Testing Counter Directly Done...")

  println("Testing Counter Via Probe...")
  // running a TestProbe from the outside
  implicit val system2 = ActorSystem("CounterDriver2")
  val cd2 = system2.actorOf(Props[CounterDriver], "CounterDriver2Main")
  val p = TestProbe()
  p watch cd2
  p.send(cd2, CounterDriver.AddNum(3))
  p.expectNoMsg(100 millis)
  p.send(cd2, CounterDriver.ValRequest)
  p.expectMsg(CounterDriver.ValReply(3))
  p.send(cd2, CounterDriver.AddNum(5))
  p.expectNoMsg(100.millis)
  p.send(cd2, CounterDriver.ValRequest)
  p.expectMsg(CounterDriver.ValReply(8))
  p.send(cd2, CounterDriver.DownRequest)
  p.expectMsg(CounterDriver.DownReply)
  p.expectTerminated(cd2)
  println("Testing Counter Via Probe Done...")
  system2.shutdown()

  // running inside a TestKit
  println("Testing Counter Inside TestKit...")
  new TestKit(ActorSystem("CounterDriver3")) with ImplicitSender {
    val cd3 = system.actorOf(Props[CounterDriver], "CounterDriver3Main")
    testActor ! TestActor.Watch(cd3) // context.watch(cd3)
    cd3 ! CounterDriver.AddNum(2)
    expectNoMsg(100 millis)
    cd3 ! CounterDriver.ValRequest
    expectMsg(CounterDriver.ValReply(2))
    cd3 ! CounterDriver.AddNum(3)
    expectNoMsg(100.millis)
    cd3 ! CounterDriver.ValRequest
    expectMsg(CounterDriver.ValReply(5))
    cd3 ! CounterDriver.DownRequest
    expectMsg(CounterDriver.DownReply)
    expectTerminated(cd3)
    system.shutdown()
  }
  println("Testing Counter Inside TestKit Done...")
  println("------------------------------")
  println("CounterDriver Testing DONE...")
}

