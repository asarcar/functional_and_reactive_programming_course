package mystl

import common._

object NLists {

  // type lessFn[T] = (T, T) => Boolean

  abstract class NList[+T] {
    def isEmpty: Boolean = this match {
      case Nil => true
      case h :: t => false
    }
    def length: Int = {
      def tot_len(l: NList[T], acc: Int): Int = {
        if (l.isEmpty) acc else tot_len(l.tail, acc + 1);
      }
      tot_len(this, 0)
    }
    def apply(n: Int): T = {
      if (n <= 0) {
        throw new IndexOutOfBoundsException("Index n outside # elements in List")
      } else if (isEmpty) {
        throw new IndexOutOfBoundsException("Index n outside # elements in List")
      } else if (n == 1) {
        head
      } else {
        tail.apply(n-1)
      }
    }
    def head: T = this match {
      case Nil => throw new NoSuchElementException("Nil.head")
      case h :: t => h 
    }
    def tail: NList[T] = this match {
      case Nil => throw new NoSuchElementException("Nil.head")
      case h :: t => t
    }
    def last: T = this match {
      case Nil => throw new NoSuchElementException("Nil.last")
      case h :: Nil => h
      case h :: t => t.last
    }
    def init: NList[T] = this match {
      case Nil => throw new NoSuchElementException("Nil.last")
      case h :: Nil => Nil
      case h :: t => h :: t.init
    }
    // Take and keep first n elements of the list
    def take(n: Int): NList[T] = {
      if ((n == 0) || (isEmpty))
        Nil
      else {
        head :: tail.take(n-1)
      }
    }

    // Drop first n elements of the list and take the rest
    def drop(n: Int): NList[T] = {
      if ((n == 0) || (isEmpty)) {
        this
      } else {
        tail.drop(n-1)
      }
    }    
    def ::[U >: T](h: U) = new ::(h, this)
    def :::[U >: T](that: NList[U]): NList[U] = that match {
      // Nil ::: this
      case Nil => this
      // (h :: t) ::: this
      case h :: t => this.:::(t).::(h)
    }

    // for comparison functions
    import math.Ordering
    // add entry in sorted list
    def insert[U >: T](entry: U)(implicit ord: Ordering[U]): NList[U] = {
      if (isEmpty) {
        NList(entry)
      } else if (ord.lt(entry, head)) {
        entry :: this
      } else {
        head :: tail.insert(entry)
      }
    }
    // merge two sorted lists: that list with this list
    def merge[U >: T](that: NList[U])(implicit ord: Ordering[U]): NList[U] = {
      if (that.isEmpty) {
        this
      } else {
        /*
         * Merge the remaining of that list (that.tail)
         * on this list inserted with that.head to current list
         */
        this.insert(that.head).merge(that.tail)
      }
    }
    def mergesort[U >: T](implicit ord: Ordering[U]): NList[U] = {
      if ((isEmpty) ||(tail.isEmpty)) {
        this
      } else {
        val len = this.length
        val leftsort = take(len/2).mergesort(ord)
        val rightsort = drop(len/2).mergesort(ord)
        leftsort.merge(rightsort)
      }
    }

    override def toString: String = this match {
      case Nil => "Nil"
      case h :: t => h + "::" + t
    }
    /*
     **
     ** Higher Order Functions
     ** *foldRight, foldLeft, reverse
     ** *def mapFun[A, B](xs: List[A], f: A => B): List[B] =
     ** *  (xs :\ List[B]()){ ?? }
     ** *def lengthFun[A](xs: List[A]): int =
     ** *  (0 /: xs){ ?? }
     ** *List.range(1, n).flatMap(i => List.range(1, i).map(x => (i, x)))
     ** *.filter(pair => isPrime(pair._1 + pair._2))
     **
     */
    /*
     * foldLeft operator symbol "/:"
     * foldRight operator symbol ":\"
     * Assuming
     * 1. l = x1 :: x2 :: x3 :: x4 :: .. :: xN :: Nil (where x1..xN: A)
     * 2. Identity: y1 = reduce(y1, Nil) and yi+1 = reduce(yi, xi) (where yi: B)
     * foldLeft computes yN+1 = ((((y1 op x1) op x2) op x3)... op xN)
     * 
     * 1. l = xN :: xN-1 :: xN-2 :: xN-3 :: .. :: x1 :: Nil (where xN..x1: A)
     * 2. Identity: y1 = reduce(y1, Nil) and yi+1 = reduce(yi, xi) (where yi: B)
     * foldRight computes yN+1 = (xN op ...(x2 op (x1 op y1)))
     */
    def /:[U](y: U)(op: (U, T) => U): U = this match {
      case Nil => y
      case h :: t => t./:(op(y, h))(op)
    }
    def :\[U](y: U)(op: (T, U) => U): U = this match {
      case Nil => y
      case h :: t => op(h, tail.:\(y)(op))
    }
    def reverse: NList[T] = {
      val seedvalue: NList[T] = Nil
      this./:(seedvalue)((list, elem) => elem :: list)
    }
    def map[U](f: T => U): NList[U] = {
      /*
       * map_acc: accumulates the result of map so that
       * the recursion of map simplifies to tail recursion
       * However: this implementation reverses the order of the
       * list: map(x1 :: x2 :: ... :: xN :: Nil) =
       * f(xN) :: f(xN-1) :: ... :: f(x1) :: Nil
       */
      def map_acc[U](f: T => U, l: NList[T], acc: NList[U]): NList[U] = {
        if (l.isEmpty)
          acc
        else
          map_acc(f, l.tail, f(l.head) :: acc)
      }
      map_acc(f, this, Nil).reverse

      /*
       * One could replace map_acc with a foldRight call which is not as efficient
       * (does not support tail recursion).
       * On the other hand, it does not reverse the order of mapped list
       * 
       * val seedvalue: NList[U] = Nil
       * this :\ (seedvalue)((elem, list) => f(elem) :: list)
       */
    }
    /*
     * Returns a list of elements in the list that pass the predOnElemFn test
     */
    def filter(predOnElemFn: T => Boolean): NList[T] = {
      if (isEmpty) {
        Nil
      } else if (predOnElemFn(head)) {
        head :: tail.filter(predOnElemFn)
      } else {
        tail.filter(predOnElemFn)
      }
    }
    def exists(predOnElemFn: T => Boolean): Boolean = {
      if (isEmpty) {
        false
      } else if (predOnElemFn(head)) {
        true
      } else {
        tail.exists(predOnElemFn)
      }
    }
    /*
     * predicate is true for all elements if there does not exist a
     * single element for which the predicate is false
     */
    def forall(predOnElemFn: T => Boolean): Boolean = {
      !exists(elem => !predOnElemFn(elem))
    }
    def foreach(elemFn: T => Unit): Unit = this match {
      case Nil => ()
      case h :: t => elemFn(h); t.foreach(elemFn)
    }
    /*
     * flatMap: concatenate the list of all lists created
     * by running the mapElemToListFn on each element of list.
     */
    def flatMap[U](mapElemToListFn: T => NList[U]): NList[U] = {
      if (isEmpty)
        Nil
      else
        mapElemToListFn(head) ::: tail.flatMap(mapElemToListFn)
    }
    /*
     * remove any duplicate entries from the list
     */
    def removeDuplicates: NList[T] = {
      if (isEmpty) {
        Nil
      } else {
        val headDedupList: NList[T] = tail.filter(x => x != head)
        head :: headDedupList.removeDuplicates
      }
    }
  }

  case object Nil extends NList[Nothing]
  case class :: [+T](h: T, t: NList[T]) extends NList[T]

  object NList {
    def apply[T](args: T*): NList[T] = {
      if (args.isEmpty) {
        Nil
      } else {
        args.head :: apply(args.tail : _*)
      }
    }
    // Generate NList of Integers in ascending order in range (being, end]
    def range(begin: Int, end: Int): NList[Int] = {
      def range_acc(begin: Int, end: Int, acc: NList[Int]): NList[Int] = {
        if ((end - begin) <= 0) {
          acc
        } else {
          range_acc(begin, end-1, end-1:: acc)
        }
      }
      range_acc(begin, end, Nil)
    }
  }

  def main(args: Array[String]) {
    println("NList creation via constructor: ")
    println("NList() = " + NList() +
      ": NList(1) = " + NList(1) +
      ": NList(1, 2) = " + NList(1, 2) +
      ": NList('raja', 'praja') = " + NList("raja", "praja"))
    println("NList.range(-2, -3) = "  + NList.range(-2, -3))
    println("NList.range(-2, 4) = "  + NList.range(-2, 4))

    println("NList Creation = 10->4->-5->.")
    val l = 10 :: 4 :: -5 :: Nil
    val l2 = 8 :: 20 :: 3 :: -12 :: Nil
    println("l(2): elem " + l(2))

    println("l.last = " + l.last + "; l.init = " + l.init)

    println("l = " + l + "; l2 = " + l2)
    println("l2.take(2) = " + l2.take(2) + "; l2.drop(2) = " + l2.drop(2))

    val l3 = l ::: l2
    println("l3 = l ::: l2 = " + l3)
    println("l3.mergesort(ascending) = " + l3.mergesort)
    println("l3.mergesort(descending) = " + l3.mergesort(Ordering[Int].reverse))
    println("l3.map(x => 2*x) = " + l3.map(x => 2*x))
    println("l3.reverse = " + l3.reverse)
    println("l3.filter(x => (x%2 == 0)) = " + l3.filter(x => (x%2 == 0)))
    println("l3.flatMap(x => NList(x, x*x)) = " + l3.flatMap(x => NList(x, x*x)))

    println("Test Covariance: ")
    import Trees._
    val y0: EmptyTree[Int] = EmptyTree()
    val y1: Node[Int] = Node(10, y0, y0)
    val y2: Tree[Int] = Node(5, y0, y0)
    println("y0 " + y0 + ": y1 " + y1 + ": y2 " + y2)

    val l0: NList[EmptyTree[Int]] = new ::(y0, Nil)
    val lNode: NList[Node[Int]] = new ::(y1, Nil)
    val lTree: NList[Tree[Int]] = new ::(y2, Nil)
    println("l0 = " + l0 + "; lNode = " + lNode + "; lTree = " + lTree)

    /*
     *  l0 and lNode are subtype of lTree2/3 as
     *  EmptyTree[Int] and Node[Int] are subtypes of Tree[Int]
     */
    val lTree2: NList[Tree[Int]] = l0
    val lTree3: NList[Tree[Int]] = lNode
    println("lTree2 " + lTree2 + ": lTree3 " + lTree3)

    println("lTree2 length: " + lTree2.length + ": lTree3 length: " + lTree3.length)
    println("lTree2.head.length: " + lTree2.head.length +
      ": lTree3.head.length: " + lTree3.head.length)
  }
}
