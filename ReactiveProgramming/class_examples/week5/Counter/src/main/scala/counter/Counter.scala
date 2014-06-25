package counter

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef

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
  def receive = {
    case Incr => {
      count += 1
      log.info("Incr: count = {}...", count)
    }
    case Get(client)  => {
      sender ! CurVal(count, client)
      log.info("Get: count = {}...", count)
    }
    case Down(client: ActorRef) => {
      sender ! DownAck(client)
      log.info("Terminating...")
      context.stop(self)
    }
  }
}
