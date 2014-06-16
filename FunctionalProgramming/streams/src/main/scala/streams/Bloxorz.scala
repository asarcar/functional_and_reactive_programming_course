package streams

/**
 * A main object that can be used to execute the Bloxorz solver
 */
object Bloxorz extends App {

  /**
   * A level constructed using the `InfiniteTerrain` trait which defines
   * the terrain to be valid at every position.
   */
  object InfiniteLevel extends Solver with InfiniteTerrain {
    val startPos = Pos(1,3)
    val goal = Pos(5,8)
  }
  
  println("Solution with Infinite Terrain: startPos=" +
    InfiniteLevel.startPos + ": goal=" + InfiniteLevel.goal + "\n" +
    InfiniteLevel.solution)

  /**
   * A simple level constructed using the StringParserTerrain 
   */
  abstract class Level extends Solver with StringParserTerrain
  
  object Level0 extends Level {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  println("Solution for \n" + Level0.level + "\n" + Level0.solution)

  /**
   * Level 1 of the official Bloxorz game
   */
  object Level1 extends Level {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }

   println("Solution for \n" + Level1.level + "\n" + Level1.solution)

    /**
    *  Level 3 of the official Bloxorz game
    */
  object Level3 extends Level {
    val level =
      """------ooooooo--
      |oooo--ooo--oo--
      |ooooooooo--oooo
      |oSoo-------ooTo
      |oooo-------oooo
      |------------ooo""".stripMargin
  }

   println("Solution for \n" + Level3.level + "\n" + Level3.solution)

  /**
    *  Level 6 of the official Bloxorz game
    */
  object Level6 extends Level {
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
  }

   println("Solution for \n" + Level6.level + "\n" + Level6.solution)

  /**
    *  Level 11 of the official Bloxorz game
    */
  object Level11 extends Level {
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
    }

   println("Solution for \n" + Level11.level + "\n" + Level11.solution)

}
