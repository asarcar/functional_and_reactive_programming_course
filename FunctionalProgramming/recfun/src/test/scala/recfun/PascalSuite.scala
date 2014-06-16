package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal

  test("pascal: col=14,row=12: throws an exception") {
    intercept[IllegalArgumentException] {
      pascal(14, 12)
    }
  }

  test("pascal: col=-ve,row=+ve: throws an exception") {
    intercept[IllegalArgumentException] {
      pascal(-3, 2)
    }
  }

  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }

  test("pascal: col=12,row=12") {
    assert(pascal(10,10) === 1)
  }

  test("pascal(5, 10) == 252") {
    assert(pascal(5, 10) === 252)
  }
}
