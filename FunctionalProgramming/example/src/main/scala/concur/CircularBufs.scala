package concur

import common._
import scala.reflect.ClassTag
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/*
 * Circular Buffer that can be concurrently accessed by producers and consumers
 */
object CircularBufs {

  class CircularBuf[T: ClassTag](N: Int) {
    var in = 0; var out = 0; var n = 0
    val buf = new Array[T](N)

    def put(elem: T): Unit =  synchronized {
      // println("->put call in=%d out=%d n=%d".format(in, out, n))
      // Wait till at least one slot is available to put element
      while (n >= N) wait()
      buf(in) = elem; n = n + 1; in = (in + 1)%N
      /*
       * Wake up waiting consumers in case this
       * is the first element added to the buffer
       */
      if (n == 1) notifyAll()
      // println("<-put call in=%d out=%d n=%d".format(in, out, n))
    }

    def get: T = synchronized {
      // println("->get call in=%d out=%d n=%d".format(in, out, n))
      // Wait till at least one slot is filled to get element
      while (n <= 0) wait()
      val res: T = buf(out); n = n - 1; out = (out + 1)%N
      /*
       * Wake up waiting producers in case this
       * is the element that frees up space in the buffer
       */
      if (n == N-1) notifyAll()
      // println("<-get call in=%d out=%d n=%d".format(in, out, n))
      res
    }
  }

  def main(args: Array[String]) {
    val nThreads = if (args.length > 0) args(0).toInt else 4
    println("CircularBuffer Test: nThreads = %d: N = %d".format(nThreads, nThreads))
    val cBuf = new CircularBuf[(Int, Int)](nThreads)
    for (i <- List.range(0, nThreads)) {
      Future {
        for (j <- List.range(i*nThreads,(i+1)*nThreads)) {
          val tup = (i, j)
          cBuf.put(tup)
          println("ThId (Prod %d) => %d".format(tup._1, tup._2))
        }
      }
    }
    // Execute gets in the main thread
    for (i <- List.range(0, nThreads*nThreads)) {
      val tup = cBuf.get
      println("Get# %d (Consumed from Prod %d) => %d".format(i, tup._1, tup._2))
    }
  }
}

