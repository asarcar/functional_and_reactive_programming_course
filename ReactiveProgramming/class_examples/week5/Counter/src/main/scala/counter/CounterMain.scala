package counter

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ReceiveTimeout
import akka.testkit.ImplicitSender
import akka.testkit.TestKit
import akka.testkit.TestProbe
import scala.concurrent.duration._

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

  // Just testing one msg
  self ! Counter.Get(self)

  def receive = {
    case AddNum(n) => {
      log.info("AddNum({}) Request...", n)
      for (i <- 0 until n)
        counter ! Counter.Incr
    }
    case ValRequest => {
      log.info("ValRequest...")
      counter ! Counter.Get(sender)
    }
    case Counter.CurVal(x, client) => {
      log.info("CurVal: count = {}...", x)
      client ! ValReply(x)
    }
    case DownRequest => {
      log.info("DownRequest...")
      counter ! Counter.Down(sender)
      context.become(completeDown)
    }
      /*
       * case ReceiveTimeout => {
       *   log.info("All work completed & terminated")
       * }
       * case Counter.DoneAck => {
       *   context.setReceiveTimeout(1 seconds)
       * }
       */
  }
  def completeDown: Receive = {
    case Counter.CurVal(x, client) => {
      log.info("CurVal: count = {}...", x)
      client ! ValReply(x)
    }
    case Counter.DownAck(client) => {
      log.info("All work completed & terminated")
      client ! DownReply
      context.stop(self)
    }
  }
}

object CounterMain extends App {
  println("CounterDriver Testing BEGIN...")
  println("------------------------------")

  println("Testing Counter Directly...")
  val sys = ActorSystem("CounterDriver")
  val cd = sys.actorOf(Props[CounterDriver])
  Thread.sleep(2000)
  sys.shutdown()
  println("Testing Counter Directly Done...")

  println("Testing Counter Via Probe...")
  // running a TestProbe from the outside
  implicit val system2 = ActorSystem("CounterDriver2")
  val cd2 = system2.actorOf(Props[CounterDriver])
  val p = TestProbe()
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
  println("Testing Counter Via Probe Done...")
  system2.shutdown()

  // running inside a TestKit
  println("Testing Counter Inside TestKit...")
  new TestKit(ActorSystem("CounterDriver3")) with ImplicitSender {
    val cd3 = system.actorOf(Props[CounterDriver])
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
    system.shutdown()
  }
  println("Testing Counter Inside TestKit Done...")
  println("------------------------------")
  println("CounterDriver Testing DONE...")
}

