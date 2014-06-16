package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = (goalBlock == b)

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   * 
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   * 
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   * 
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: List[Move]):
      Stream[(Block, List[Move])] = {
    /*
     * First convert the legalneighbors: list of (blk, mov) to
     * a stream and then map to record history of moves that one
     * needed to get to the blk position:
     * Note: ((b.legalNeighbors) map {
     *         case (blk, mov) => (blk, mov :: history) }).toStream
     *       }
     * would be more expensive as we will compute all the legal neighbors
     * map to record history and then stream.
     */
    b.legalNeighbors.toStream map {case (blk, mov) => (blk, mov :: history)}
  }

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]):
      Stream[(Block, List[Move])] = {
    /*
     * We will discard any neighbor that contains a block in the
     * explored Set
     */
    neighbors filterNot { case (blk, lmov) => explored contains blk }
  }

  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   * 
   * The blocks in the stream `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   * 
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   * 
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   * 
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    if (initial.isEmpty) Stream.empty
    else {
      /*
       * BFS(Seeds, Explored)
       * 1. Seeds = Assume initial stream starts with the seed nodes from where
       *    to initiate the BFS search.
       * 2. Nbrs = The new neighbors of *all* the elements of the initial stream
       *    are the next set of nodes.
       * 3. New Seeds = Nbrs, New Explored = Nbrs + Explored
       * 4. Result = Initial streamAppend BFS(New Seeds, New Explored)
       */
      val newInitial: Stream[(Block, List[Move])] = for {
        (b, history) <- initial
        nbrs = neighborsWithHistory(b, history)
        newNbrs = newNeighborsOnly(nbrs, explored)
        newNbr <- newNbrs
        /*
         * Ideally we should expand "explored" to include the newNbrs
         * right now i.e. before any other call to newNbrs.
         * However, I am not clear how to do so. As such, we will
         * expand the "explored" Set only when making the next
         * recursive call to function from
         */
      } yield newNbr

      val newExplored = explored ++ (newInitial map {case(blk, lmov) => blk}).toSet

      initial #::: from(newInitial, newExplored)
    }
  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = 
    from(((startBlock, List()) #:: Stream.empty), Set(startBlock))

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {
    for {
      path <- pathsFromStart
      if (goalBlock == path._1)
    } yield path
  }

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = {
    if (pathsToGoal.isEmpty) List()
    else pathsToGoal.head._2.reverse
  }
}
