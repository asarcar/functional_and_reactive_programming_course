import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}

/** Contains basic data types, data structures and `Future` extensions.
 */
package object nodescala {

  /** Adds extensions methods to the `Future` companion object.
   */
  implicit class FutureCompanionOps[T](val f: Future.type) extends AnyVal {

    /** Returns a future that is always completed with `value`.
     */
    def always[T](value: T): Future[T] = {
      val p = Promise[T]()
      p.success(value)
      p.future
    }

    /** Returns a future that is never completed.
     *
     *  This future may be useful when testing if timeout logic works correctly.
     */
    def never[T]: Future[T] = Promise[T]().future // complete not called

    /** Given a list of futures `fs`, returns the future holding the list of values of all the futures from `fs`.
     *  The returned future is completed only once all of the futures in `fs` have been completed.
     *  The values in the list are in the same order as corresponding futures `fs`.
     *  If any of the futures `fs` fails, the resulting future also fails.
     */
    def all[T](fs: List[Future[T]]): Future[List[T]] = {
      /*
       * arg = List(F(T1) :: F(T2) :: F(TN):: Nil)
       *   res = F(List(T1, T2, TN))
       * if (arg == Nil) res  = F(List())
       * 
       * res = Future(await(F[T1]) :: await(F(T2 :: ... :: TN)))
       * 
       * List(F[T1], F[T2], F[T3], ...., F[TN]) => F[List(T1, T2, ..., TN)]
       * all: List() = F[List()] = futureNill
       */
      val futureNil: Future[List[T]] = Future.always(List())
      fs.foldRight(futureNil) {
        /*
         * F[Ti] op F[List(Ti+1,..., TN)] = F[List(Ti,...,TN)]
         */
        (futElem: Future[T], acc: Future[List[T]]) => {
          for {
            elem <- futElem
            listElem <- acc
          } yield {
            elem :: listElem
          }
        }
      }
    }

    /** Given a list of futures `fs`, returns the future holding the value of the future from `fs` that completed first.
     *  If the first completing future in `fs` fails, then the result is failed as well.
     *
     *  E.g.:
     *
     *      Future.any(List(Future { 1 }, Future { 2 }, Future { throw new Exception }))
     *
     *  may return a `Future` succeeded with `1`, `2` or failed with an `Exception`.
     */
    def any[T](fs: List[Future[T]]): Future[T] = {
      val p = Promise[T]()
      fs.foreach(p.tryCompleteWith(_))
      p.future
    }

    /** Returns a future with a unit value that is completed after time `t`.
     */
    def delay(t: Duration): Future[Unit] = {
      Future {
        try{
          blocking{
            /*
             * Same as a wait call for duration t on a Future (should never complete)
             * Better than Thread.sleep as sleep does not accept duration less
             * than milli secs.
             * Having said that, one may argue whether we should even bother
             * to support delay in any units less than milli secs considering
             * the scheduler accuracy may not even be relevant in micro or nano
             * seconds.
             */
            Await.ready(Future.never[Unit], t)
          }
        } catch{
          case t: TimeoutException => // ok!
        }
      }
    }

    /** Completes this future with user input.
     */
    def userInput(message: String): Future[String] = Future {
      blocking {readLine(message)}
    }

    /** Creates a cancellable context for an execution and runs it.
     */
    def run()(f: CancellationToken => Future[Unit]): Subscription = {
      /*
       * 1. Generate a cancellation token that may be unsubscribed
       * 2. Start the asynchronous computation by calling 'f'
       * 3. Return the Subscription that may be used by clients
       *    to stop the computation 'f'
       * TODO: The call to f is not "exception protected"
       */
      val subscriptionValue = CancellationTokenSource()
      f(subscriptionValue.cancellationToken)
      subscriptionValue
    }

    /*
     * Example Usage: run, subscription, cancellation token, ...
     * 
     * val working = Future.run() { ct =>
     *   Future {
     *     while (ct.nonCancelled) {
     *       println("working")
     *     }
     *     println("done")
     *   }
     * }
     * Future.delay(5 seconds) onSuccess {
     *  case _ => working.unsubscribe()
     * }
     * 
     */
  }

  /** Adds extension methods to future objects.
   */
  implicit class FutureOps[T](val f: Future[T]) extends AnyVal {

    /** Returns the result of this future if it is completed now.
     *  Otherwise, throws a `NoSuchElementException`.
     *  
     *  Note: This method does not wait for the result.
     *  It is thus non-blocking.
     *  However, it is also non-deterministic -- it may throw or return a value
     *  depending on the current state of the `Future`.
     */
    def now: T = f.value match {
      case Some(futureResult) => futureResult match {
        case Success(value) => value
        case Failure(exception) => throw exception
      }
      case None => throw new NoSuchElementException("Future.now failed as it is incomplete")
    }

    /** Continues the computation of this future by taking the current future
     *  and mapping it into another future.
     * 
     *  The function `cont` is called only after the current future completes.
     *  The resulting future contains a value returned by `cont`.
     */
    def continueWith[S](cont: Future[T] => S): Future[S] = {
      val p = Promise[S]()
      f onComplete {
        case tryVal => {
          Try(cont(f)) match {
            case Success(sVal) => p.success(sVal)
            case Failure(exception) => p.failure(exception)
          }
        }
      }
      p.future
    }

    /** Continues the computation of this future by taking the result
     *  of the current future and mapping it into another future.
     *  
     *  The function `cont` is called only after the current future completes.
     *  The resulting future contains a value returned by `cont`.
     */
    def continue[S](cont: Try[T] => S): Future[S] = {
      val p = Promise[S]()
      f onComplete {
        case tryVal => {
          Try(cont(tryVal)) match {
            case Success(sVal) => p.success(sVal)
            case Failure(exception) => p.failure(exception)
          }
        }
      }
      p.future
    }
  }

  /** Subscription objects are used to be able to unsubscribe
   *  from some event source.
   */
  trait Subscription {
    def unsubscribe(): Unit
  }

  object Subscription {
    /** Given two subscriptions `s1` and `s2` returns a new composite subscription
     *  such that when the new composite subscription cancels both `s1` and `s2`
     *  when `unsubscribe` is called.
     */
    def apply(s1: Subscription, s2: Subscription) = new Subscription {
      def unsubscribe() {
        s1.unsubscribe()
        s2.unsubscribe()
      }
    }
  }

  /** Used to check if cancellation was requested.
   */
  trait CancellationToken {
    def isCancelled: Boolean
    def nonCancelled = !isCancelled
  }

  /** The `CancellationTokenSource` is a special kind of `Subscription` that
   *  returns a `cancellationToken` which is cancelled by calling `unsubscribe`.
   *  
   *  After calling `unsubscribe` once, the associated `cancellationToken` will
   *  forever remain cancelled -- its `isCancelled` will return `false.
   */
  trait CancellationTokenSource extends Subscription {
    def cancellationToken: CancellationToken
  }

  /** Creates cancellation token sources.
   */
  object CancellationTokenSource {
    /** Creates a new `CancellationTokenSource`.
     */
    def apply(): CancellationTokenSource = new CancellationTokenSource {
      val p = Promise[Unit]()
      val cancellationToken = new CancellationToken {
        def isCancelled = p.future.value != None
      }
      def unsubscribe() {
        p.trySuccess(())
      }
    }
  }
}
