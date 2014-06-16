package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  /**
    *  Level 3 of the official Bloxorz game
    */
  trait Level3 extends SolutionChecker {
    val level =
      """------ooooooo--
      |oooo--ooo--oo--
      |ooooooooo--oooo
      |oSoo-------ooTo
      |oooo-------oooo
      |------------ooo""".stripMargin

    val optsolution = List(
      Right, Up, Right, Right, Right, Up, Left, Down, Right, Up,
      Up, Right, Right, Right, Down, Down, Down, Right, Up)
  }

  /**
    *  Level 6 of the official Bloxorz game
    */
  trait Level6 extends SolutionChecker {
    val level =
      """-----oooooo----
      |-----o--ooo----
      |-----o--ooooo--
      |Sooooo-----oooo
      |----ooo----ooTo
      |----ooo-----ooo
      |------o--oo----
      |------ooooo----
      |------ooooo----
      |-------ooo-----""".stripMargin
    val optsolution = List(
      Right, Right, Right, Down, Right, Down, Down, Right, Down, Down,
      Right, Up, Left, Left, Left, Up, Up, Left, Up, Up,
      Up, Right, Right, Right, Down, Down, Left, Up, Right, Right,
      Down, Right, Down, Down, Right)
  }

  /**
    *  Level 11 of the official Bloxorz game
    */
  trait Level11 extends SolutionChecker {
    val level =
      """-oooo-------
      |-oToo-------
      |-ooo--------
      |-o---oooooo-
      |-o---oo--oo-
      |Soooooo--ooo
      |-----o-----o
      |-----oooo--o
      |-----ooooooo
      |--------ooo-""".stripMargin
    val optsolution = List(
      Right, Right, Right, Right, Up, Left, Down, Down, Down, Right,
      Right, Right, Down, Left, Up, Left, Left, Left, Up, Up,
      Right, Up, Right, Right, Down, Right, Up, Left, Left, Left,
      Down, Down, Left, Left, Left, Up, Up, Right, Up, Up,
      Left, Down, Right, Up, Right, Down, Left)

    }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)) === true, "0,0")
      assert(terrain(Pos(4,11)) === false, "4,11")
      assert(terrain(Pos(2,-2)) === false, "2,-2")
      assert(terrain(Pos(5,4)) === false, "5,4")
      assert(terrain(Pos(4,7)) === true, "4,7")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos === Pos(1,1))
      assert(goal === Pos(4, 7))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === goalBlock,
        "solution=" + solution + ": solve(solution)=" + solve(solution) +
          ": goalBlock=" + goalBlock)
      assert(solution.length === 7,
        "solution=" + solution + ": optsolution=" + optsolution)
      assert(solution === optsolution, 
        "solution=" + solution + ": optsolution=" + optsolution)
    }
  }

  test("optimal solution for level 3") {
    new Level3 {
      assert(solve(solution) === goalBlock,
        "solution=" + solution + ": solve(solution)=" + solve(solution) +
          ": goalBlock=" + goalBlock)
      assert(solution.length === 19,
        "solution=" + solution + ": optsolution=" + optsolution)
      assert(solution === optsolution,
        "solution=" + solution + ": optsolution=" + optsolution)
    }
  }
   
  test("optimal solution for level 6") {
    new Level6 {
      assert(solve(solution) === goalBlock,
        "solution=" + solution + ": solve(solution)=" + solve(solution) +
          ": goalBlock=" + goalBlock)
      assert(solution.length === 35,
        "solution=" + solution + ": optsolution=" + optsolution)
      assert(solution === optsolution,
        "solution=" + solution + ": optsolution=" + optsolution)
    }
  }
   
  test("optimal solution for level 11") {
    new Level11 {
      assert(solve(solution) === goalBlock,
        "solution=" + solution + ": solve(solution)=" + solve(solution) +
          ": goalBlock=" + goalBlock)
      assert(solution.length === 47,
        "solution=" + solution + ": optsolution=" + optsolution)
      assert(solution === optsolution,
        "solution=" + solution + ": optsolution=" + optsolution)
    }
  }
}
