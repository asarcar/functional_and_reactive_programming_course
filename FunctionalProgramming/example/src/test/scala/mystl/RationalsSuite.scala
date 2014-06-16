package mystl

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RationalsSuite extends FunSuite {
  import Rationals._

  trait TestSets {
    val r_1 = new Rational(1)
    val r_2_by_3 = new Rational(2, 3)
    val r_3_by_4 = new Rational(3, 4)
    val r_minus_3_by_4 = -r_3_by_4
    val r_minus_1_by_12 = new Rational(-1, 12)
    val r_1_by_2 = new Rational(1, 2)
    val r_8_by_9 = new Rational(8, 9)
    val r_11_by_1 = new Rational(11, 1)
  }
  test("Rational: Test Operations: 'binary {==, +, -, *, /}' & 'unary -'") {
    new TestSets {
      assert((r_2_by_3 == r_2_by_3), "Test ==: '2/3 == 2/3'")
      assert((-r_3_by_4 == r_minus_3_by_4), "Test Unary Minus: '-3/4 == -3/4'")
      assert(((r_2_by_3 - r_3_by_4) == r_minus_1_by_12), "Test Subtract: '2/3 - 3/4 = -1/12")
      assert((r_2_by_3*r_3_by_4 == r_1_by_2), "Test Multiplication: '2/3*3/4 = 1/2'")
      assert((r_2_by_3/r_3_by_4 == r_8_by_9), "Test Division: '(2/3)/(3/4) = 8/9'")
      assert((((r_2_by_3 + r_3_by_4 - r_2_by_3*r_3_by_4)/(r_3_by_4 - r_2_by_3)) == r_11_by_1),
        "'(2/3 + 3/4 - (2/3*3/4))/((3/4)-(2/3)) = 11/1'")
    }
  }
}
