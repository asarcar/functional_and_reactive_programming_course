package mystl

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TreesSuite extends FunSuite {
  import Trees._

  trait TestSets {
    val x0: Tree[Int] = EmptyTree()
    val x1: Tree[Int] = Node(10, x0, x0)
    val x2 = x1.insert(5).insert(20)
  }

  test("Test: contains") {
    new TestSets {
      assert(!x0.contains(10), "EmptyTree contains nothing")
      assert(x1.contains(10), "Tree with 10 should contain 10")
    }
  }

  test("Test: insert") {
    new TestSets {
      assert(x2.insert(100).contains(100), "Tree with 100 insert contains 100")
    }
  }
}
