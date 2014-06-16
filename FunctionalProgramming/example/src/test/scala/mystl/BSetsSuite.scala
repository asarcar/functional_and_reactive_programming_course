package mystl

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BSetsSuite extends FunSuite {
  import BSets._

  trait TestSets {
    val ex0: BSetP[Int] = NullBSet
    val ex1 = ex0.incl(8) // {8}
    val ex2 = ex0.incl(4).incl(21).incl(6) // {4 21 6}
    val ex3 = ex0.incl(5).incl(20).incl(3) // {5 20 3}
    val ex4 = ex2 union ex3
  }

  test("BasicTests: isEmpty & numElems") {
    new TestSets {
      assert(ex0.isEmpty, "NullBSet.isEmpty true")
      assert(!ex1.isEmpty, "BSet.isEmpty false")
      assert(ex0.numElems === 0, "NullBSet numElems zero")
      assert(ex2.numElems === 3, "ex2 " + ex2 + " numElems 3")
      assert(!ex1.contains(2), "ex1 contains 2 false")
      assert(ex3.contains(20), "ex3 " + ex3 + " contains 20")
    }
  }
  test("Modification Tests: incl and union Tests") {
    new TestSets {
      assert(ex3.incl(2).contains(2), "ex3 " + ex3 + " incl 2 contains 2")
      assert(ex4.numElems === 6, "ex4 " + ex4 + " (ex2 union ex3) numElems 6")
      assert(ex4.contains(21) && ex4.contains(20) && !ex4.contains(0), "ex4 " + ex4 + " contain 21 & 20 not 0")
    }
  }
}
