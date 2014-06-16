package mytypes

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NatsSuite extends FunSuite {
  import Nats._

  trait TestSets {
    val nZero = Zero
    val nOne = new Succ(Zero)
    val nTwo = new Succ(nOne)
  }
  test("Natural: Basic Comparisons") {
    new TestSets {
      assert(nZero == nZero, "Test Zero == Zero")
      assert(nOne != nTwo, "nOne != nTwo")
      assert(nTwo > nOne, "nTwo > nOne")
      assert(nTwo >= nZero, "nTwo >= nZero")
      assert(nZero < nTwo, "nZero < nTwo")
      assert(nZero <= nOne, "nZero <= nOne")
    }
  }

  test("Natural: Arithmetic (+/-) Operations") {
    new TestSets {
      assert(nOne - nZero == nOne, "nOne - nZero == nOne")
      assert(nOne + nZero == nOne, "nOne + nZero == nZero")
      val nFive = nTwo + nTwo + nOne
      assert(nTwo - nOne == nOne, "nTwo - nOne == nOne")
      assert(nFive + nTwo == nTwo + nTwo + nTwo + nOne,
        "nFive + nTwo == nTwo + nTwo + nTwo + nOne")
      assert(nFive - nTwo == nTwo + nOne,
        "nFive - nTwo == nTwo + nOne")
    }
  }
}
