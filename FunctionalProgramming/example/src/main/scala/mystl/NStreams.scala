package mystl

import NLists._
import common._

object NStreams {
  abstract class NStream[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: NStream[T]
    def contains[U >: T](x: U): Boolean = {
      if (isEmpty) {
        false
      } else if (head == x) {
        true
      } else {
        tail.contains(x)
      }
    }
    def filter(p: T => Boolean): NStream[T] = {
      if (isEmpty) {
        NStream.Empty
      } else if (p(head)) {
        NStream.cons(head, tail.filter(p))
      } else {
        tail.filter(p)
      }
    }
    def apply(index: Int): T = {
      if ((index < 0) || (isEmpty)) sys.error("index: out of range")
      else if (index == 0) head
      else tail.apply(index-1)
    }
    override def toString = if (isEmpty) "." else head + "->?"
  }
  object NStream {
    object Empty extends NStream[Nothing] {
      def isEmpty: Boolean = true
      def head: Nothing = sys.error("Empty.head")
      def tail: Nothing = sys.error("Empty.tail")
    }
    def range(from: Int, to: Int): NStream[Int] = {
      if (from >= to) {
        Empty
      } else {
        cons(from, range(from + 1, to))
      }
    }
    def cons[T](hd: T, tl: => NStream[T]) = new NStream[T] {
      def isEmpty: Boolean = false
      def head: T = hd
      lazy val tail = {/*println("Tail Evaluation: NStream=" + tl);*/ tl}
    }
  }

  def main(args: Array[String]): Unit = {
    def fib(a: Int, b: Int): NStream[Int] = {
      NStream.cons(a, fib(b, a+b))
    }

    val fibs: NStream[Int] = fib(1,1)

    val numFib = if (args.length > 0) args(0).toInt else 5

    var fiblist = fibs
    for (i <- List.range(1, numFib+1)) {
      println("Evaluating fibs(%d)".format(i))
      for (j <- List.range(1, i+1)) {
        require(!fiblist.isEmpty, "fiblist is Empty")
        println("fib(%d)=%d".format(j, fiblist.head))
        fiblist = fiblist.tail
      }
      fiblist = fibs
    }

    println("fibs.contains(21): " + fibs.contains(21))
    val evenRange = NStream.range(10, 20).filter(x => x%2 == 0)
    println("EvenRange (1) element: " + evenRange(1))
  }
}
