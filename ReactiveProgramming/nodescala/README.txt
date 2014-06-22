Part 1: Extending Futures
In the first part of the exercise you will extend the Futures and Promises API with some additional methods. We will define these methods in the file package.scala.

Extension Methods on Futures
In Scala you can add missing methods to existing classes and singleton objects. Lets say you want to have a new Future factory method userInput in the Future companion object that expects user input and completes the future with the user input once the ENTER key was pressed. The Future companion object is already baked into the standard library, so you cannot add a method there directly. Here is an example how you can add userInput using extension methods:

implicit class FutureCompanionOps(f: Future.type) extends AnyVal {
  def userInput(message: String): Future[String] = Future {
    readLine(message)
  }
}
The implicit modifier on the class declaration above means that the compiler will generate an implicit conversion from the Future companion object to the FutureCompanionOps object. The declaration above is desugared into:

class FutureCompanionOps(f: Future.type) extends AnyVal {
  def userInput(message: String): Future[String] = Future {
    readLine(message)
  }
}
implicit def f2ops(f: Future.type) = new FutureCompanionOps(f)
This implicit conversion will be called every time you call a non-existing method on the Future companion object â Future.userInput thus automatically becomes f2ops(Future).userInput. The extends AnyVal part is just an optimization telling the compiler to avoid instantiating the FutureCompanionOps object where possible and call its methods directly.

The bottomline is that whenever you want to add missing methods to an already existing class implementation, you should use this pattern.

Lets see a simple example of how to implement an additional combinator on an instance of Future[T]. This combinator should take the current future f and the target future that and produce a new future that is completed with the value of the current future if and only if the that future is completed successfully. If that is not completed successfully, the resulting future should be completed with its exception. We will call this combinator ensuring. Here is how you could implement it:

implicit class FutureOps[T](f: Future[T]) {
  def ensuring[S](that: Future[S]): Future[T] = {
    val p = Promise[T]()

    f onComplete {
      case tryValue =>
        that onComplete {
          case Success(_) =>
            p.complete(tryValue)
          case Failure(exception) =>
            p.failure(exception)
        }
    }

    p.future
  }
}
You start by creating a promise object p. The method ensuring will return a future corresponding to that promise. Then we install a callback to f using onComplete â when f completes with either success or a failure tryValue (either Success or Failure), it will install an additional callback to that. This additional callback will complete the promise p with either the exception if that fails, or with tryValue if that succeeds.

Companion objects often contain factory methods for object creation. You will now add the following methods to the Future companion object â see the ScalaDoc comments in the source code for an explanation what each of these must do:

def always[T](value: T): Future[T] // hint - use a Promise to implement this!
def never[T]: Future[T] // hint - use a Promise to implement this!
def any[T](fs: List[Future[T]]): Future[T] // hint - use a Promise
def all[T](fs: List[Future[T]]): Future[List[T]] // hint - see the lectures
def delay(t: Duration): Future[Unit]
In the same way, add the following methods to Future[T] instances (again, see the ScalaDoc comments in the source code):

def now: T
def continueWith[S](cont: Future[T] => S): Future[S]
def continue[S](cont: Try[T] => S): Future[S]
We will use the factory methods and combinators defined above later in the exercise.

Use whatever tool you see most appropriate for the job when implementing these factory methods â existing future combinators, for-comprehensions, Promises or async/await. You may use Await.ready and Await.result only when defining the delay factory method and the now method on Futures. All the methods except delay should be non-blocking. The delay may block the execution thread of its future until the specified time period elapses, but it should not block the caller thread.

Note that whenever you have a long-running computation or blocking make sure to run it inside the blocking construct. For example:

blocking {
  Thread.sleep(1000)
}
is used to designate a piece of code which potentially blocks, allowing the thread scheduler to add additional threads and resolve potential deadlocks. Example: lets say you have a future f that waits for a resource or a monitor condition that can only be fulfilled by some other future g. In that case, the part of the code in f that does the waiting should be wrapped in the blocking, otherwise the future g might never be run.

Adding Cancellation
Standard Scala Futures cannot be cancelled. Instead, cancelling an asynchronous computation requires a collaborative effort, in which the computation that is supposed to be cancelled periodically checks a condition for cancellation.

In this part of the exercise we will develop support for easier cancellation. We introduce the following traits:

trait CancellationToken {
  def isCancelled: Boolean
}
The CancellationToken is an object used by long running asynchronous computation to periodically check if they should cancel what they are doing. If isCancelled returns true, then an asynchronous computation should stop.

trait Subscription {
  def unsubscribe(): Unit
}
Subscriptions are used to unsubscribe from an event. Calling unsubscribe means that the Subscription owner is no longer interested in the asynchronous computation, and that it can stop.

trait CancellationTokenSource extends Subscription {
  def cancellationToken: CancellationToken
}
The CancellationTokenSource is a special kind of Subscription that returns a cancellationToken which is cancelled by calling unsubscribe. After calling unsonce, the associated cancellationToken will forever remain cancelled.

Here is how to implement the default CancellationTokenSource:

object CancellationTokenSource {
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
In the above implementation, a Promise p is used to implement the CancellationTokenSource. This interface requires implementing 2 methods - cancellationToken and unsubscribe. The unsubscribe method is meant to be called by clients to let the computation know that it should stop. It tries to complete the promise p in case it wasnât already completed. The cancellationToken method simply returns a CancellationToken that queries the state of the promise p in its isCancelled method. The computation can periodically query isCancelled to check if it should be cancelled.

We use the above-defined method to implement a method run on the Future companion object that starts an asynchronous computation f taking a CancellationToken and returns a subscription that cancels that CancellationToken:

def run()(f: CancellationToken => Future[Unit]): Subscription = ???
Clients can use Future.run as follows:

val working = Future.run() { ct =>
  Future {
    while (ct.nonCancelled) {
      println("working")
    }
    println("done")
  }
}
Future.delay(5 seconds) onSuccess {
  case _ => working.unsubscribe()
}
Part 2: An Asynchronous HTTP Server
Finally, you have everything you need to write an asynchronous HTTP Server. The HTTP server will asynchronously wait on some port for incoming HTTP requests and then respond to them by sending some text or HTML back. You will be able to open your browser at the address http://localhost:8191/someRelativepath and see how your server responds to you!

Open the file nodescala.scala. There you will find the following declarations:

type Request = Map[String, List[String]]
type Response = Iterator[String]
Each HTTP request consists of a sequence of headers that are key-value pairs. Same keys may occur in multiple headers in the same HTTP requests, so we encode the request as a Map mapping a key to a List of all corresponding header values.

Each HTTP response will be just some text. We could thus represent Response with a String. We will instead represent it with an Iterator[String] so that we can respond chunk by chunk if the entire text or an HTML document is very big.

The trait Exchange is used to write your response back to the user using the write method. Whenever you use it, donât forget to close it by calling the close method.

Once you implement your server, you will be able to instantiate a server listening at a port p like this:

val myServer = new NodeScala.Default(p)
After that, you will be able to instruct the server to listen for requests at a specific relative path:

val homeSubscription = myServer.start("/home") {
  req => "Have a nice day!".split(" ").iterator
}
HTTP Listener
Every HTTP server creates multiple Listener objects, one for every relative path on which it listens for requests. These Listeners wait for incoming HTTP requests and can create Futures with the subsequent requests. However, the Listener, basing its implementation on standard HTTP support on the JVM, internally has a callback-based API. This is unfortunate, since such an API allows us to install callbacks using createContext and remove them using removeContext. We would instead like to represent incoming requests as Future objects, so we will use this callback based API to have callbacks complete a Future returned from the Listener.

Open the nodescala.scala file. Your first task is to implement the nextRequest method in the Listener trait. This method will return a Future containing a pair of the Request, and an Exchange object used to write the response back to the HTTP client.

In the nextRequest method, the Listener creates an empty Promise p to hold the (Request, Exchange) pair, is a callback function using the createContext method that will complete the promise with the request and then remove itself using removeContext, and returns the Future of the Promise p. This pattern in which a callback completes a Promise to translate an event into a Future is ubiquitous in reactive programming with Futures.

Implement the nextRequest method.

Hint: make sure you understand how the createContext and removeContext methods of the HttpServer class work.

The HTTP Server
In this part you will implement the two server methods start and respond of the trait NodeScala in the file nodescala.scala.

The respond method is used to write the response back to the client using an exchange object. While doing so, this method must periodically check the token to see if the response should be interrupted early, otherwise our server might run forever!

private def respond(exchange: Exchange, token: CancellationToken, response: Response): Unit
Your first task is to implement the method respond.

To start the HTTP server, we declare a single method start in file nodescala.scala:

def start(relativePath: String)(handler: Request => Response): Subscription
This method takes a relativePath at which a request arrives and a request handler. It creates a listener at relativePath and runs the following cancellable computation using Future.run: if the computation is not cancelled, awaits the nextRequest from the listener, responds to it asynchronously using respond and keeps repeating this until the computation is cancelled.

Finally, method start returns a Subscription that cancels all asynchronous computations at this relative path.

Your task is to implement start using Futures in the following way:

create and start an http listener
create a cancellation token to run an asynchronous computation (hint: use the Future.run companion method)
in this asynchronous computation, while the token is not cancelled, await the next request from the listener and then respond to it asynchronously schematic
have the method start return a subscription that cancels the http listener, the server loop and any responses that are in progress (hint: use one of the Subscription companion methods)
Instantiating the Server
Finally, you can instantiate the server in the file Main.scala:

Create a server myServer on port 8191 and start listening on a relative path /test with a subscription myServerSubscription
Create a userInterrupted future that is completed when the user presses ENTER, continued with a message "You entered... " (use the userInput future)
Create a timeOut future that is completed after 20 seconds, continued with a message "Server timeout!"
Create a terminationRequested future that is completed once any of the two futures above complete
Once the terminationRequested completes, print its message, unsubscribe from myServer and print "Bye!"
Hint: where possible, use the previously defined Future factory methods and combinators.

Open your browser and type http://localhost:8191/test into the address bar. Congratulations â your server is fully functional!

FAQ:
----
Assignment 3 FAQ Help

Hello all,

This is the FAQ document for week 3 - we're listing answers to common questions here. Make sure you read this before posting a question - maybe the answer's already here!

If you don't find an answer here and want to ask a question on the forum, be sure to provide enough background! Questions like: "Text XXX is not passing in the grader for me, what did I do wrong?" are not very helpful. A minimal context would at least provide the relevant grader output log, and some hint of how you attempted to implement that particular part of the exercises - both the staff and other students will be much more likely to be able to help you.



0. Where can I find some code to juggle with? I want code examples!

Try this nice collection of worksheets: https://github.com/greenTara/reactive-examples/tree/master/src/main/scala/future

Forks and pull requests with improvements are welcome!



1. What is a Promise? What is a Future? What does the Future type have to do with doing an asynchronous computation like this  Future { ... }? And how is this different from async ?

First of all, there is a difference between the type  Future[T] and the computation started with the  Future  block.

The type Future[T] designates a value of type T that might not be available now, but will become available at some point in the future. This type captures the concept of time in your programs - the future is initially not completed, and then becomes completed at some time. And because the value of the future is not always available, you cannot always get the value immediately by calling some method on instances of the type Future[T]. Instead, you must install callbacks to this type using onComplete , or, more conveniently, you should use map , flatMapor filter to transform one future in some other future - see lectures for more details on that.

Question is - how can I even obtain values of this type Future[T]? There are several ways. First of all, you can use future combinators. If you have an instance of a future that you obtained from somewhere else, for example, a 3rd party API with methods that return futures, you can transform those futures into something else by using  for-comprehensions, for example. Details of how to do this are shown in the lectures.

But what if you don't have any future instance at all to start with, so you cannot apply future combinators? You can start of an asynchronous computation that will potentially execute on a different thread and that will complete the future. This is done with the Future { ... } block - but it is not to be confused with the Future[T] type! The Future[T] might not really involve any asynchronous computation, it just designates the possibility of it.

Next question is - if Future[T] can be completed differently than by starting an asynchronous computation, then how? This is where the promises and the Promise[T] type comes in. The Promise[T] is a single-assignment variable - an object which initially has no value, but to which you can write some value of type  T only once using methods like complete or success, and then it has that value forever. Instances of the type Promise[T] have a method called future (note the lower case) that returns the Future[T] corresponding to that promise. As soon as you write to a promise, its corresponding future will be completed. If you think about it, this allows you to obtain a completed future involving no asynchronous computation whatsoever.

Finally, the async call is similar to the Future { ... } block in that it starts an asynchronous computation. However, its intent is somewhat different. As noted in the lectures, in concurrent reactive programming on the JVM you should block the thread as least as possible, which is what happens when you use the blocking Await.result calls. Nevertheless, awaiting on a value is a concept familiar to most programmers and people do it nonetheless. Wouldn't it be great if you could somehow write code that seemingly waits for a value like normal, sequential, imperative code, but is actually asynchronous under the hood and does not block the thread? It turns out this is possible and this is what async does for you - it causes the compiler to transform the seemingly sequential-looking code that blocks into a chain of future computations that are executed asynchronously . This mind-boggling transformation is something you don't have to understand the details of, but you can visit the Scala SIP page if you're really interested in how it's implemented.

It should be enough to watch the lectures to understand how to use the async block to start an asynchronous computation and how to use await within it to wait for future values that are yet to complete. However, we don't expect you to use async- as explained in the other question, you can avoid it altogether and just use futures and promises. 

The methods above are all legal ways of obtaining futures. You should think about which of them suits you mostly for the task at hand, and use that one. 


2. How can I uses promises to create futures? Why is there a hint in some exercises to use a promise when I must return a future? 

Each Promise[T]  has its own Future[T]  -- to describe it symbolically, think of using these two as a pipe and 2 people trying to communicate through it. The person P at the promise end of the pipe can push a piece of paper with a message into it. The person F at the future end has to wait until that piece of paper comes out at the other side. How can the person F at the future end wait for it? He can sit there all day and wait until the message (hopefully) arrives, doing nothing (analogous to Await.result ), or he can be smart about it, tie his dog with a leash around the pipe and go do some useful work (onComplete ). A dog will start barking once he sees a message falling out of the tube, and the person F can then come back to read the message, after having done some useful work in the meantime :) 

def personP(sink: Promise[String]) {
  sink.success("Hi! Are there any dogs on the other side? I'm afraid of them... :(")
}

def personF(source: Future[String]) {
  source onComplete { // person F leaves a smart dog that knows how to format strings Scala-way
    case msg => println(s"Woof, man! '$msg'")
  }
}

def communicate() {
  val pipe = Promise[String]()
  Future { personP(pipe) }
  Future { personF(pipe.future) }
}



3. Can we use Thread.sleep?

Yes you can when defining the delay, but it should not be your general practice. It's only necessary for some future combinators, but very few of them. In fact, you can use any construct from scala.concurrent that results in delaying for that amount of time.

Just make sure that when you read the note about blocking!




4. Should I use async or future combinators and promises?

Although async/await was shown in the lectures, we realize that it might confuse most people. All the exercises in this assignment can be solved using only promises, asynchronous computations started with the  Future { ... } block and combinators on futures like map or filter. Furthemore, async/await is still an experimental Scala feature. We therefore advise you to avoid async/await, in the interest of keeping things simple.

However, if you would like to use async/await to solve the exercise, feel free to do so! 


5. Why does async not work in a value class - what are all these errors with nested classes not allowed in value classes? 

The Scala Async feature is nearing its first release and is partly experimental.

As a workaround for using Scala Async in value classes, consider moving the async invocation outside of the value class using the following trick:

class FutureOps[T](f: Future[T]) extends AnyVal {
  def foo: Future[T] = fooImpl(f)
}

def fooImpl(f: Future[T]) = async {
  val x = await { f }
  throw new Exception
}
If that or other code juggling and refactoring does not solve the problem, consider using regular functional combinators like  map ,   flatMap ,   continueWith  and the rest of their friends.




6. What's a good reference for futures and promises?

This is: http://docs.scala-lang.org/overviews/core/futures.html Also the API docs can help: http://www.scala-lang.org/api/current/index.html\#scala.concurrent.Future

And don't forget the lectures! :)




7. What is this continuing stuff? What are the semantics of continueWith and friends, and how should it handle exceptions?

Methods like Future.continueWith are closely related to map methods. The difference is that they map not only the success value T, i.e. they don't take f: T => S, but they're designed to be able to map over failures as well - they take f: Try[T] => S and f: Future[T] => S.

This means that you can call them like this:

def bar(x: Int) = future { if (x > 0) x else throw new Exception } continue {
  case Success(x) => x + 1
  case Failure(t) => 0
}
All Future combinators should handle exceptions in a reasonable way. This means that if the user gives them arbitrary blocks of code to execute, they should be aware that those blocks of code also potentially throw. When in doubt, consult the implementation of flatMap in the Scala standard library:

https://github.com/scala/scala/blob/master/src/library/scala/concurrent/Future.scala#L239 

In that link we can see the common pattern when designing most future combinators:

create a promise  p 
install an  onComplete callback to the current future to run function f on the result of the future, and use the return value of  f to somehow complete the promise p 
make sure to catch any exception while executing f 
return the future of  p 


8. What should be semantics of now?

It should throw an exception if the future is not completed, and return the value if the future is completed. And if the future is completed with an exception - well, the only thing to do is to throw the exception back at the user.



9. What should be semantics of always?

It should return a future that is always completed with the specified value, i.e. as soon as the future is constructed and returned it should already be completed. It should not be returned and then completed after the method  always already returned.




10. Why can't I mix Futures with Lists in my for-comprehensions?

Because the flatMap on the List is defined to accept only functions that return other Lists, roughly speaking, and flatMap on the Future is defined to only accept functions that return futures.

The reason that is done this way is because List would have to know the semantics of futures and vice versa for this to be implemented correctly. These two abstractions model very different things -- a list has no idea that a future is asynchronous.

But, to solve this issue, you can always convert a list to a future of that list. Get it? ;)



11. What is cancellation useful for? Why is CancellationTokenSource recommended to use a Promise?

CancellationTokens and CancellationTokenSources are used to communicate in the opposite direction than the one you do when you use Future s. Clients ask for Future s and wait until they get some result back from the computation, but they cannot write to the Future . On the other hand, clients can try to unsubscribe from the computation using CancellationTokenSource s and in this way write back to the computation. The computation itself cannot write to the CancellationToken , only check if they are cancelled.

On the JVM you're strongly advised to use proper synchronization when sharing mutable state between the threads, to learn more about this you can try to find resources on the Java Memory Model.

You could either use the @volatile annotation or the synchronized keyword to ensure that the inner state of the CancellationTokenSource is properly accessed, but the simplest thing is to reuse the logic you already have in Promise. Promises are single-assignment variables that are already thread-safe.



12. What does the blocking call do? When do I have to use it?

The explanation is fairly technical. It has to do with how thread pools work on the JVM. When a typical thread pool is created on the JVM, it contains some fixed number of threads. These threads are used to work on client-submitted tasks, for example, computations that have to execute asynchronously when you call the Future  block - for this reason, these threads are called the worker threads. Each worker thread has a queue of tasks and continually polls this queue to see if there are new tasks to execute. In some cases, worker threads may even share this queue(s) by taking each other's tasks. This all works out nicely until it happens that all the worker threads execute tasks with code which blocks on some condition. At that point, even if new tasks arrive, no worker thread will be able to execute them, because it is blocking. Assume that all the threads are blocking for user input, for example. If you call userInput   4 times on a 4 processor machine (there are as many worker threads as processors, by default), all the worker threads will wait for user input. If you start additional futures after that, they will not be executed until the user enters some input.

All this shows that blocking is generally not a good idea with the JVM and concurrent reactive programming. But sometimes you need it! There is a way out of this, however - typically, thread pools can add new worker threads to execute the tasks if they can detect that all the threads are blocked. Various thread pools do this in different ways, but in general they need a hint. This is where the  blocking  construct comes in. Any time a future executes code inside  blocking  , it will notify the thread pool to watch out - potentially new threads should be created to prevent thread starvation.

You have to watch out when implementing  delay  and  userInput  for this reason.

See more information here: http://stackoverflow.com/questions/19681389/use-case-of-scala-concurrent-blocking



13. Why don't I see any output from the server - it immediately exits? What are daemon threads?

On the JVM the threads can have the daemon status. At any point during the execution of the JVM, there are multiple threads active (meaning that they started running, and are now either running or blocking, but have not yet terminated). If all the threads in the active state are daemon threads, the JVM will exit.

The scala.concurrent package uses an abstraction called an ExecutionContext that represents thread pools. The thread pool is just a bunch of threads that can be used to execute some asynchronous computation if the user requested it. The default ExecutionContext in Scala sets its worker threads to be daemons. This means that you don't have to manually shut down your default execution context if your program used it before it ended. If the main thread exits the main function, and there are no other non-daemon threads remaining, the JVM process should terminate.

Since the only thread running in your Server assignment application is the main thread (unless you changed something), the application should indeed end, just as you are describing! Why, then, are some people see a different behaviour?

The reason is that most people run their server using the run task in SBT. SBT is a build tool that runs in the JVM and works by compiling all your source code into bytecode, then loading that bytecode into its own JVM with custom  ClassLoaders, and then execute the main method of your application. By default, SBT does not spawn a new JVM process to run your Server application. When SBT's is done executing main, it simply returns to the interactive shell. Its own non-daemon threads, thus, still exist and prevent your server, which runs only on the ExecutionContext's daemon threads, from dying.

If you want to verify the above statements, simply set the SBT's fork property to true:

set fork := true                           
This will instruct SBT to start a new JVM process each time you call run. Setting fork back to false will make the server behave as expected.

If your Server application is immediately dying after running it, then it's possible that you're running the Server from the command line or from Eclipse, so your application dies immediately. Try to run it from SBT, and see what happens. If nothing else works, you can just add an  Await.ready  at the end of the main method.



14. What is this  Future.run method and why is it useful?

This method creates a cancellation token source and runs a block of code f  that takes a cancellation token. The block of code can start an asynchronous computation that occasionally checks the cancellation token to see if it should stop.

This method  Future.run then returns a subscription that can cancel the above-mentioned cancellation token.

It is useful because it provides a syntactically lightweight way to start a long-running asynchronous computation and then, if and when necessary, cancel it by unsubscribing from it. The example in the instructions does exactly that:

val working = Future.run() { ct =>
  Future {
    while (ct.nonCancelled) {
      println("working")
    }
    println("done")
  }
}
Future.delay(3 seconds) onSuccess {
  case _ => working.unsubscribe() // if loop still not completed, cancel it
}


15. What is this  Future.type type, and why are there 2 implicit classes for extending futures?

In Scala, each class may have a companion object, a special singleton object with the "same name" that usually contains factory methods and other useful utilities related to that class. Class Future is no exception in that sense - in the Scala standard library you can find the following declarations:

trait Future[T] { ... }

object Future { // this is the companion object
  def apply[T](body: =>T) = { ... }
  // and a lot more ...
}
Futures have the type, well, Future[T], but how would you write the type of the future companion object? The Future.type is the type of the Future companion object. In Scala you can always use the annotation .type on an object to refer to the type of any object. Note that this is a handy way to refer to the type of any companion object in general:

class Foo

object Foo
Writing the : Foo type ascription would refer to the type belonging to the class Foo. To refer to the type of the companion object and disambiguate from Foo, you need to write : Foo.type - that is pretty much the only way to refer to that type, since it does not have its own name.

Why are there 2 classes extending methods on futures? Note that FutureOps adds additional methods to Future[T] objects, so you can call:

def foo[T](f: Future[T]) = f.myCustomMethod("Argument 1")
FutureCompanionOps adds methods to the Future companion object, so you can do this:

Future.myCustomWayToCreateAFuture("Argument 1")


16. Why is Future.always useful? Why would I use it if its value is already precomputed?

 Future.always is a way to lift a normal value T to a Future[T] value. This lifting pattern is something you will see often in functional programming, so remember it well!

To make its usefulness more apparent - imagine that your API method should either call some Web service or look in the cached responses to see if the Web service was already queried with that request. In the first case you have to return a future  Future[String]  of a response, and in the second you need to return a response  String  right away from your cache. Future.always can help you solve this tricky type situation.

def query(request: String): Future[String] =
  if (cache.contains(request)) Future.always(cache(request))
  else Future { askServer(request) }


17. Wait! Won't calling this createContext and removeContext methods create a short time interval in the server during which the server is not listening to incoming requests?

The idea of this exercise is to show you asynchronous programming with futures on an easy, but real-world example and make sure that it's as easy to understand. That does not mean this is the best web server that exists out there. We were aware about this problem when designing the assignment - we even implemented a dataflow stream that served as a buffer of requests to avoid having some requests potentially lost. However, we did not add this to the assignment so that we don't throw you off - you're not implementing a full blown Apache server here.

Note that you will never notice this in practice - the period between calling  createContext  and  removeContext  is probably too short to notice this.


