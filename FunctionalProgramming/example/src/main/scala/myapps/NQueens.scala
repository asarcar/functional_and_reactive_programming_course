package myapps

import common._
import mystl.NLists._

object NQueens {
  def queens(n: Int): NList[NList[Int]] = {
    def placeQueens(k: Int): NList[NList[Int]] =
      if (k == 0) NList(NList())
      else for { queens <- placeQueens(k - 1)
        column <- NList.range(1, n + 1)
        if isSafe(column, queens, 1) } yield column :: queens
    placeQueens(n)
  }
  /*
   * isSafe: Queen in row k wrt list with queens List 
   * - false: list of queens is empty: false
   * - false: queen in row k (col) is in same column wrt queen in row i (queens.head)
   * - false: queen in row k (col) is diagonal to queen in row i
   * - isSafe from the remaining queens in queens list
   */
  def isSafe(col: Int, queens: NList[Int], delta: Int): Boolean =
    if (queens.isEmpty)
      true
    else if ((queens.head - col).abs == 0)
      false
    else if ((queens.head - col).abs == delta)
      false
    else
      isSafe(col, queens.tail, delta + 1)

  def getDim(args: Array[String]): Int = {
    if (args.isEmpty)
      9
    else
      args.head.toInt
  }

  def main(args: Array[String]) {
    val n = getDim(args)
    println("queens list: (n = " + n + "): ")
    queens(n).foreach(x => println("NList: " + x))
  }
}
