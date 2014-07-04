package counter

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.PoisonPill

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scala.util.Random

/*
 * Companion Object: Define all messages
 */
object Counter {
  case class Incr
  case class Get(client: ActorRef)
  case class CurVal(v: Int, client: ActorRef)
  case class Down(client: ActorRef)
  case class DownAck(client: ActorRef)
}

class Counter extends Actor with ActorLogging {
  import Counter._
  var count = 0
  val rnd = new Random()

  def receive = {
    case Incr => {
      count += 1
      log.debug("Incr: count = {}...", count)
    }
    case Get(client)  => {
      sender ! CurVal(count, client)
      log.debug("Get: count = {}...", count)
    }
    case Down(client: ActorRef) => {
      log.debug("Terminating...")
      sender ! DownAck(client)
      // goDown
      context.stop(self)
    }
  }

  def goDown = context.system.scheduler.
    scheduleOnce(100 + rnd.nextInt(900) millis){self ! PoisonPill}
}
