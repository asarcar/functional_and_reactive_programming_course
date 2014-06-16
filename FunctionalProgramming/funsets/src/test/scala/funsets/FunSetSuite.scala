package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
    *  Tests are written using the "test" operator and the "assert" method.
    */
  /*
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }
   */
  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  /*
  test("adding ints") {
    assert(1 + 2 === 3)
  }
   */
  
  import FunSets._

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val u12 = union(s1, s2)
    val u23 = union(s2, s3)
    val u123 = union(u12, u23)

    val set_a1 = union(singletonSet(20), singletonSet(-30))
    val set_a2 = union(singletonSet(11), singletonSet(-9))
    val set_a = union(set_a1, set_a2)

    /* set_b has all elements of set_a but squared */
    val set_b1 = union(singletonSet(400), singletonSet(900))
    val set_b2 = union(singletonSet(121), singletonSet(81))
    val set_b = union(set_b1, set_b2)
  }

  test("contains basic") {
    assert(contains(x => true, 100))
  }
  
  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains relevant elements") {
    new TestSets {
      assert(contains(u12, 1), "1 union 2 contains 1")
      assert(contains(u12, 2), "1 union 2 contains 2")
      assert(!contains(u12, 3), "1 union 2 does not contain 3")
    }
  }

  test("intersect contains relevant elements") {
    new TestSets {
      assert(contains(intersect(u12, u23), 2), "12 intersect 23 contains 2")
      assert(!contains(intersect(u123, u23), 1), "123 intersect 23 does not contain 1")
    }
  }

  test("diff contains relevant elements") {
    new TestSets {
      assert(contains(diff(u12, u23), 1), "12 diff 23 contains 1")
      assert(!contains(diff(u12, u23), 2), "12 diff 23 does not contain 2")
      assert(contains(intersect(diff(u123, u23), s1), 1), "(123 diff 23) intersect s3 contains 1")
    }
  }

  test("filter contains relevant elements") {
    new TestSets {
      assert(contains(filter(u23, x => ((x == 1) || (x == 2))), 2), "(23 filter 1 or 2 contains 2")
      assert(!contains(filter(u23, x => ((x == 1) || (x == 2))), 1), "(23 filter 1 does not contains 1")
    }
  }

  test("forall, exists, and map tests") {
    new TestSets {
      assert(forall(set_a, x => ((x >= -100) && (x <= 100))),
        "forall: set_a {20, -30, 11, -9}, all numbers between -100 to 100")
      assert(!forall(set_b, x => ((x >= -100) && (x <= 100))),
        "forall: set_b {400, 900, 121, 81}, all numbers not between -100 to 100")
      assert(exists(set_a, x => (x%7 == 6)),
        "exists: set_a {20, -30, 11, -9}, number exists that mod 7 is 6")
      assert(!exists(set_b, x => (x%7 == 6)),
        "exists: set_b {400, 900, 121, 81}, no number exists that mode 7 is 6")
      assert(contains(map(s1, x => 2*x), 2), "s1: {1} => map(s1, 2*s1) contains 2")
      assert(same(map(set_a, x => x*x), set_b),
        "map(set_a, squarefn) same as set_b: set_a {20, -30, 11, -9} set_b {400, 900, 121, 81}")
    }
  }
}
