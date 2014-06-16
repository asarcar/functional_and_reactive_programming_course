package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("Future.always should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) === 517)
  }

  test("Future.never should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Future.all completes when all members complete") {
    val listFutures = List(Future.always(10), Future.always(20), Future.always(5))
    val futureLists = Future.all(listFutures)

    /*
     * 100 milli second (instead of 0 nanos) to execute the instructions to
     * compile the list: even when each member of the list (future)
     * returns immediately one needs to provide some time to execute all the
     * instruction chain of the functions.
     */
    assert(Await.result(futureLists, 100 millis) === List(10, 20, 5))
  }

  test("Future.all does not complete when any member does not complete") {
    val listFutures = List(Future.never[Int], Future{10},
      Future{5}, Future.never[Int])
    val futureLists = Future.all(listFutures)

    try {
      assert(Await.result(futureLists, 1 second) === List(),
        "Future.all should not even complete if any future does not complete")
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Future.any completes when any member completes") {
    val fut = Future.any(List(Future.never[Int], Future{10},
      Future{5}, Future.never[Int]))

    assert(List(10, 5) contains Await.result(fut, 100 millis),
      "Any future failed with result not match any of the members")
  }

  test("Future.any completes when any member throws an exception") {
    case class MyException(msg: String) extends Exception(msg)

    val fut = Future.any(List(
      Future.never[Int],
      Future{throw MyException("Bad-10")},
      Future{throw MyException("Bad-20")},
      Future.never[Int]))

    try {
      assert(Await.result(fut, 100 millis) === 0, // any condition that is false
        "Any future with only exceptions should not return any value")
    } catch {
      case MyException(s) => assert(List("Bad-10", "Bad-20") contains s)
    }
  }

  test("Future.now returns value on completion, exception, or incomplete computation") {
    case class MyException(msg: String) extends Exception(msg)
    val fNowSuccess = Future.always(10)
    val fNowException = Future{throw new MyException("Future throw MyException")}
    val fNowIncomplete = Future.never[Int]

    assert(fNowSuccess.now === 10,
      "Future.now failed on a future with available result")
    try {Await.result(fNowException, 100 millis)}
    catch {case MyException(s) => /* ok */}
    try {fNowException.now}
    catch {case MyException(s) => assert(s === "Future throw MyException")}

    try {val valNowIncomplete = fNowIncomplete.now}
    catch {case t: NoSuchElementException =>}
  }

  test("Future.continueWith on computation returns result of function") {
    val fMult = Future.always(10).continueWith(f => f.now*25)
    assert(Await.result(fMult, 100 millis) === 250,
      "Future.continueWith: using results from both futures")
  }
  test("Future.continue on computation returns result of function") {
    val fSecond = Future{throw new Exception("Future.continue test")}.
      continue(tryVal => 250)
    assert(Await.result(fSecond, 100 millis) === 250,
      "Future.continue failure on first future: result based on function")

    val fThird = Future.never[Int].continue(tryVal => 250)
    try {
      assert(Await.result(fThird, 1 second) === 0, // any result that is not valid
        "Future.continue should not return as first future never completed")
    } catch {
      case t: TimeoutException => // ok
    }
  }

  test("Future.delay completes after 'delay' duration") {
    val futsec = Future.delay(1 second)
    try {
      Await.result(futsec, 700 millis)
    } catch {
      case t: TimeoutException => // ok
    }
    Await.result(futsec, 700 millis)
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("Future.run spawns infinite computation and stopping it as well") {
    val p = Promise[Int]()
    val sub: Subscription =
      Future.run() {
        (ct) => Future {
          while (ct.nonCancelled) {
            blocking {
              Thread.sleep(1) // sleep for 1 milli second
            }
          }
          p.success(123)
        }
      }
    try {
      Await.result(p.future, 900 millis)
    } catch {
      case t: TimeoutException =>
    }
    sub.unsubscribe()
    assert(Await.result(p.future, 100 millis) === 123,
      "Future.run continuing computation even after unsubscribe")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2.1", "StrangeValue2.2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content === expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))
    test(immutable.Map("WorksForFour" -> List("Indeed", " ", "Always works.",
      " ", "Trust me.")))

    dummySubscription.unsubscribe()
  }

}




