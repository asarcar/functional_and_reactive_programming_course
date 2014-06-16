package mystl

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NListsSuite extends FunSuite {
  import NLists._

  trait TestSets {
    val l0 = Nil
    val l1 = 10 :: 4 :: -5 :: Nil 
    val l2 = 3 :: -1 :: 4 :: Nil
    val l3 = l1 ::: l2
  }

  test("NList Creation Tests: NList(a, b, c, ...)") {
    new TestSets {
      val x0 = NList()
      val x1 = NList(1, 2, 3)
      val x2 = NList("a", "b", "c")
      assert(x0.isEmpty, "NList().isEmpty true")
      assert(x1(2) === 2, "NList(1, 2, 3)(2) is 2")
      assert(x2(3) === "c" , "NList('a', 'b', 'c')(3) is 'c'")
      assert(x1.length === x2.length,
        "length NList(1, 2, 3) and NList('a', 'b', 'c') same")
    }
  }
  test("NList Creation Test: NList.range(begin, end)") {
    new TestSets {
      val x3 = NList.range(-2, -2)
      val x4 = NList.range(-2, 4)
      assert(x3.isEmpty, "NList.range(-2, -2) == Nil")
      assert(x4.head === -2, "NList.range(-2, 4).head == -2")
      assert(x4.last === 3, "NList.range(-2, 4).last == 3")
      assert(x4.length === 6, "NList.range(-2, 4).length == 6")
    }
  }
  test("NList BasicTests: isEmpty & length") {
    new TestSets {
      assert(l0.isEmpty, "Nil.isEmpty true")
      assert(!l1.isEmpty, "Cons.isEmpty false")
      assert(l0.length === 0, "Nil.length zero")
      assert(l2.length === 3, "l2 lendth 3")
      assert(l3.length === 6, "l3 lendth 3")
    }
  }
  test("NList Exception Tests: head/tail/last/init of Nil List") {
    new TestSets {
      intercept[NoSuchElementException] {
        l0.head
      }
      intercept[NoSuchElementException] {
        l0.tail
      }
      intercept[NoSuchElementException] {
        l0.last
      }
      intercept[NoSuchElementException] {
        l0.init
      }
    }
  }
  test("NList(n) Exception Test: Index out of bound") {
    new TestSets {
      intercept[IndexOutOfBoundsException] {
        l0(0)
      }
      intercept[IndexOutOfBoundsException] {
        l1(4)
      }
    }
  }
  test("NList(n): Index Test: list pos 0/1/2") {
    new TestSets {
      assert(l2(1) === 3, "l2 (3->-1->4->.) pos 0: 3")
      assert(l2(2) === -1, "l2 (3->-1->4->.) pos 1: -1")
      assert(l3(3) === -5, "l3 (10->4->-5->3->-1->4->.) pos 3: -5")
    }
  }
  test("NList: MergeSort Test") {
    new TestSets {
      import math.Ordering
      assert(l3.mergesort(Ordering[Int])(5) === 4,
        "l3.mergesort ascending (-5->-1->3->4->4->10->.) val index 5: 5")
      assert(l3.mergesort(Ordering[Int].reverse)(5) === -1,
        "l3.mergesert descending (.<--5<--1<-3<-4<-4<-10) val index 5: -1")
    }
  }
  test("NList: Reverse Test") {
    new TestSets {
      assert(l3.reverse(2) === -1, "l3.reverse (10<-4<--5<-3<--1<-4<-.)(2) === -1")
    }
  }
  test("NList: Map Test") {
    new TestSets {
      assert(l3.map(x => x*x)(3) === 25, "l3.map(sq) (100->16->25->9->1->16->.)(3) === 25")
    }
  }
  test("NList: Filter Test") {
    new TestSets {
      assert(l3.filter(x => (x%2 == 0))(3) === 4,
        "l3.filter(x => (x%2 == 0))(3) == 4: where l3: (10->4->-5->3->-1->4->.)")
    }
  }
  test("NList: flatMap Test") {
    new TestSets {
      assert(l3.flatMap(x => NList(x, x*x))(6) === 25,
        "l3.flatMap(x => NList(x, x*x))(6) == 25: where l3: (10->4->-5->3->-1->4->.)")
    }
  }
}
