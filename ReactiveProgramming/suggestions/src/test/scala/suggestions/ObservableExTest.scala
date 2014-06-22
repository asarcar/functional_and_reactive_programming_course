package suggestions
package gui

import language.postfixOps
import scala.collection._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala.{Observable, Subscription}
import org.scalatest._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ObservableExTest extends FunSuite {

  test("Future to Observable") {
    val observed = mutable.Buffer[Int]()
    val observer = observablex.ObservableEx(Future{20}) subscribe {
      observed += _
    }
    // some time for the future to be delivered
    blocking{Thread.sleep(100)}

    assert(observed === Seq(20), observed)
  }

  object wikiApi extends WikipediaApi {
    /* few dummy definitions to enable compilation and testing */
    def wikipediaSuggestion(term: String) = Future{List()}
    def wikipediaPage(term: String) = Future{""}
  }
  import wikiApi._

  test("Sanitize Observable") {
    val observed = mutable.Buffer[String]()
    val obs: Observable[String] = Observable(1, 2, 3).
      map(i => (i*2).toString + " " + (i*2).toString)
    val observer = obs.sanitized subscribe {
      observed += _
    }
    // some time for the future to be delivered
    blocking{Thread.sleep(100)}
    assert(observed === Seq("2_2", "4_4", "6_6"), observed)
  }

  test("Recovered Observable") {
    val observed = mutable.Buffer[Try[Int]]()
    val ex: Throwable = new Exception("Int")
    val obs: Observable[Int] =
      Observable(1, 2) ++ Observable(ex) ++ Observable(3)
    val observer = obs.recovered subscribe {
      observed += _
    }
    // some time for the observable
    blocking{Thread.sleep(100)}
    assert(observed === Seq(Success(1), Success(2), Failure(ex)),
      observed)
  }

  test("timedOut Observable") {
    val observed = mutable.Buffer[Long]()
    val obs = Observable.interval(300 milli)
    val observer = obs.timedOut(1) subscribe {
      observed += _ + 1
    }
    // some time for the observable
    blocking{Thread.sleep(1100)}
    assert(observed === Seq(1, 2, 3), observed)
  }

  test("concatRecovered Observable") {
    val observed = mutable.Buffer[Try[Int]]()
    val ex: Throwable = new Exception("ObsInt")
    val obs = Observable(1, 2, 3) concatRecovered (
      num => {
        if (num == 2) Observable(num, num+1) ++ Observable(ex)
        else Observable(num, num*2)
      }
    )
    val observer = obs subscribe {
      observed += _
    }
    // some time for the observable
    blocking{Thread.sleep(100)}
    assert(observed === Seq(Success(1), Success(2),
      Success(2), Success(3), Failure(ex),
      Success(3), Success(6)), observed)
  }
}
