package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

  test("countChange: 0 CHF") {
    assert(countChange(0,List(1,5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: empty list of coin denominations") {
    assert(countChange(10,List()) === 0)
  }

  test("countChange: coin denominations repeated") {
    assert(countChange(4,List(2, 1, 2, 1)) === 3)
  }

  test("countChange: -ve money: no change") {
    assert(countChange(-1, List(1, 5, 10)) === 0)
  }

  test("countChange: degenerate case: -ve or zero coin throws exception") {
    intercept[IllegalArgumentException] {
      countChange(1, List(-1, 1, 0, 5, 10))
    }
  }
}
