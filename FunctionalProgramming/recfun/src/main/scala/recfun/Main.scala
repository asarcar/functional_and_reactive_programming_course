package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("balance: \"(asdascf(df)(65325)($%%((fds(d)&^&)#)))()sbc\" = " +
      balance("(asdascf(df)(65325)($%%((fds(d)&^&)#)))()sbc".toList))

    println("countChange: (300,List(500,5,50,100,20,200,10)) = " +
      countChange(300,List(500,5,50,100,20,200,10)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ((c < 0) || (r < 0)) 
      /* Not defined for -ve numbers */
      throw new IllegalArgumentException("pascal undefined: -ve args")
    else if ((c == r) || (c == 0))
      /* numbers at the edge of pyramid is always 1 */
      1
    else if (c > r)
      /* Not defined for column > row */
      throw new IllegalArgumentException("pascal undefined: column > row")
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
    * Dump Pascal
    */
  def dump_pascal(r: Int) = {
    println("Pascal's Triangle")
    println("-----------------")
    for (row <- 0 to r) {
      for (s <- 0 to r-row-1) print("   ")
      for (col <- 0 to row)
        print(pascal(col, row) + "     ")
      println()
    }
    println("-----------------")
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_depth(depth: Int, chars: List[Char]): Boolean = {
      /* depth: running count of # of '(' - # of ')' */
      if (depth < 0)
        /* more ')' braces encountered: balance fails */
        false
      else if (chars.isEmpty)
        (depth == 0)
      else if (chars.head == '(')
        balance_depth(depth+1, chars.tail)
      else if (chars.head == ')')
        balance_depth(depth-1, chars.tail)
      else
        balance_depth(depth, chars.tail)
    }

    balance_depth(0, chars)
  }

  /* Deduplicates a list of sorted coins */
  def dedup(coins: List[Int]): List[Int] = {
    val scoins = coins.sorted
    if ((scoins.isEmpty) || (scoins.tail.isEmpty))
      /* no coins in list or just one coin left: dedup done */
      scoins
    else if (scoins.head == scoins.tail.head)
      /* duplication: eliminate head from list of coins */
      dedup(scoins.tail)
    else
      /* list includes head and dedup of remaining list */
      scoins.head :: dedup(scoins.tail)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    /* partitions rem_money (>= 0) based on a deduplicated positive
     * set of coin denominations.
     */
    def countNumChange(remaining_money: Int, dscoins: List[Int]): Int = {
      if (remaining_money == 0)
        /* combination is valid and adds up to money: accept this combination */
        1
      else if ((remaining_money < 0) || (dscoins.isEmpty))
        /* current combination does not add up to change OR
         * money left but we are out of coins to contribute to combination:
         * discard this combination */
        0
      else {
        /*
         * Assume lowest denominator coin = dscoins.head = min
         * # Change Cases:
         * a. Includes min and remaining money is created from
         *    denominations >= min
         * b. Does not include min and all money is created from
         *    denominations other than min.
         */
        countNumChange(remaining_money - dscoins.head, dscoins) +
        countNumChange(remaining_money, dscoins.tail)
      }
    }

    /* sanitize args: remove duplicated coin denominations: sort coin list */
    val dscoins = dedup(coins)

    /* sanity checks */
    if ((!dscoins.isEmpty) && (dscoins.head <= 0))
      /* Not defined for zero or -ve coins */
      throw new IllegalArgumentException("countChange bad args: <=0 coin denomination")
    else if (money == 0)
      0
    else
      countNumChange(money, dscoins)
  }
}
