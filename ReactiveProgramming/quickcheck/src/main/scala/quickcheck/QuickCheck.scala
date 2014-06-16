package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  /*
   * Heap Operations
   * 
   * def empty: H // the empty heap
   * def isEmpty(h: H): Boolean // whether the given heap h is empty
   * def insert(x: A, h: H): H // the heap resulting from inserting x into h
   * def findMin(h: H): A // a minimum of the heap h
   * def deleteMin(h: H): H // a heap resulting from deleting a minimum of h
   * def meld(h1: H, h2: H): H // the heap resulting from merging h1 and h2
   *
   * Suggested Properties:
   * 1. If you insert any two elements into an empty heap,
   *    finding the minimum of the resulting heap should
   *    get the smallest of the two elements back.
   * 2. If you insert an element into an empty heap, then delete the minimum,
   *    the resulting heap should be empty.
   * 3. Given any heap, you should get a sorted sequence of elements
   *    when continually finding and deleting minima. (Hint: recursion
   *    and helper functions are your friends.)
   * 4. Finding a minimum of the melding of any two heaps should return a
   *    minimum of one or the other.
   */

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  lazy val genList: Gen[List[Int]] = for {
    v <- arbitrary[Int]
    l <- oneOf(value(List()), genList)
  } yield v :: l

  implicit lazy val arbList: Arbitrary[List[Int]] = Arbitrary(genList)

  def heapToList(h: H): List[A] = {
    if (isEmpty(h)) List()
    else findMin(h) :: heapToList(deleteMin(h))
  }

  def listToHeap(l: List[Int]): H = {
    if (l.isEmpty) empty
    else insert(l.head, listToHeap(l.tail))
  }

  property("empty: isEmpty(empty) == true") = forAll { a: Int =>
    isEmpty(empty)
  }

  property("insert: isEmpty(a+h)==false") = forAll { (h: H, a: Int) =>
    !isEmpty(insert(a, h))
  }

  property("min: min(a+h)==a") = forAll { a: Int =>
    findMin(insert(a, empty)) == a
  }

  property("min: min(a+h) == {a,min(h)}") = forAll { (a: Int, h: H) =>
    findMin(insert(a, h)) == (if (isEmpty(h)) a else math.min(a, findMin(h)))
  }
  
  property("min: min(a+b+empty)==min(a,b)") = forAll { (a: Int, b: Int) =>
    findMin(insert(a, insert(b, empty))) == math.min(a, b)
  }

  property("delMin: delMin(a+empty) == empty") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("delMin: min(delMin(a+b+empty))  == {max(a,b)}") = forAll { (a: Int, b: Int) =>
    findMin(deleteMin(insert(a, insert(b, empty)))) == math.max(a, b)
  }

  property("delMin: min(delMin(h1, h2) == 2nd highest element") = forAll {
    (a1: Int, a2: Int, b1: Int, b2: Int, hp1: H, hp2: H) =>
    val h1 = insert(a1, insert(a2, hp1))
    val h2 = insert(b1, insert(b2, hp2))
    val h = meld(h1, h2)
    val m = findMin(h)
    val m_2 = findMin(deleteMin(h))
    val m1 =  findMin(h1)
    val m1_2 = findMin(deleteMin(h1))
    val m2 =  findMin(h2)
    val m2_2 = findMin(deleteMin(h2))

    m_2 == math.min(math.min(m1_2, m2_2), math.max(m1, m2))
  }

  property("meld: isEmpty(h+empty) == isEmpty(empty+h))") = forAll { h: H =>
    isEmpty(meld(h, empty)) == isEmpty(meld(empty, h))
  }

  property("meld: min(h1 + h2) = min(min(h1),min(h2)") = forAll { (a1: A, a2: A, h1: H, h2: H) =>
    val minOverAll = findMin(insert(a1, insert(a2, meld(h1, h2))))
    val min1 = findMin(insert(a1, h1))
    val min2 = findMin(insert(a2, h2))
    minOverAll == math.min(min1, min2)
  }

  property("heapSort should give a sorted list") = forAll { (h: H) =>
    val l = heapToList(h)
    (l.isEmpty) || (l.zip(l.tail).forall(elem => (elem._1 <= elem._2)))
  }

  property("heapSort of list should be same as list sorted") = forAll { (l: List[Int]) =>
    val h = listToHeap(l)
    heapToList(h) == l.sortWith((x1, x2) => (x1 < x2))
  }
}
