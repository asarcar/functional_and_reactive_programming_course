package nodescala

import scala.async.Async.{async, await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

object Main {

  def main(args: Array[String]) {
    /*
     * 1. instantiate the server at 8191, relative path "/test",
     *    and have the response return headers of the request
     */
    val myServer = new NodeScala.Default(8191)
    val myServerSubscription = myServer.start("/test") {
      request => {
        for ((k, v) <- request)
        yield (k + ": " + v.mkString(" ") + "\r\n")
      }.toIterator
    }

    /*
     * 2. create a future that expects some user input `x`
     *    and continues with a `"You entered... " + x` message
     */
    val userInterrupted: Future[String] = 
      Future.userInput("Press ENTER to terminate: ") continueWith {
        fut => s"You entered... ${fut.now}"
      }

    /*
     * 3. create a future that completes after 20 seconds
     *    and continues with a `"Server timeout!"` message
     */
    val timeOut: Future[String] = 
      Future.delay(30 seconds) continueWith {
        fut => "Server timeout!"
      }

    /*
     *  4. create a future that completes when either 20 seconds elapse
     *     or the user enters some text and presses ENTER
     */
    val terminationRequested: Future[String] = 
      Future.any(List(userInterrupted, timeOut))

    /*
     * TO IMPLEMENT
     * 5. unsubscribe from the server
     */
    terminationRequested onSuccess {
      case msg => {
        /*
         * 1. print message
         * 2. unsubscribe from myServer
         * 3. print "Bye!"
         */
        println("\n" + msg)
        myServerSubscription.unsubscribe()
        println("Bye!")
      }
    }

    Await.ready(terminationRequested, Duration.Inf)
  }
}
