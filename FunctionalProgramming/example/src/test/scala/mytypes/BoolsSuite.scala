package mytypes

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BoolsSuite extends FunSuite {
  import Bools._

  trait TestSets {
    val f: Bool = FALSE
    val t: Bool = TRUE
  }
  test("Bool: Logic Operations") {
    new TestSets {
      assert((f == FALSE).toBoolean, "f FALSE")
      assert((!t == FALSE).toBoolean, "!t FALSE")
      assert(((f && t) == FALSE).toBoolean, "f && t FALSE")
      assert(((f || t) == TRUE).toBoolean, "f || t == TRUE")
      assert((((f && (f || t)) || (t && !f)) == TRUE).toBoolean,
        "(f && (f || t)) || (t && !f) = TRUE")
    }
  }
  test("Bool: Comparison Operations") {
    new TestSets {
      assert(((f == t) == FALSE).toBoolean, "f == t FALSE")
      assert(((f < t) == TRUE).toBoolean, "f < t == TRUE")
      assert(((f >= t) == FALSE).toBoolean, "f >= t == FALSE")
    }
  }
}
