package myapps

import common._

object StreamApps {
  def primeStream: Stream[Int] = {
    /*
     * Generate a stream of all numbers starting with n
     */
    def from(n: Int): Stream[Int] = n #:: from(n+1)

    def removeMultiple(lnum: Stream[Int], num: Int) =
      lnum filter (_ % num != 0)

    /*
     * Prime Number Stream:
     * 1. Start with S: all natural numbers starting from 2
     * 2. First Number in list is a prime number: n
     * 3. Remove from S all multiples of n
     * 4. Go to (2)
     * First Prime is first number.
     * Second Prime is first number of the remaining list with multiples
     * of first number deleted from the list
     */
    def eliminateNonPrimes(lnum: Stream[Int]): Stream[Int] =
      lnum.head #:: eliminateNonPrimes(lnum.tail filter (_ % lnum.head != 0))

    eliminateNonPrimes(from(2))
  }

  /*
   * fixedPoint of a function f is where f(x) == x
   */
  def fixedPoint(f: Double => Double): Double = {
    def betterEstimate(cur: Double) = (cur + f(cur))/2.0
    /*
     * Create a stream of successive estimates of the fixed Point Function
     */
    def genEstStream(est: Double): Stream[Double] =
      est #:: genEstStream(betterEstimate(est))
    /*
     * Return the first value in the stream where the % variation from 
     * the prior estimate is < .01%
     */
    def finalVal(s: Stream[Double]): Double = {
      val est = s.head
      val nextest = s.tail.head
      if (math.abs((est - nextest)/est) < 0.000001)
        est
      else
        finalVal(s.tail)
    }

    val estStream = genEstStream(1)
    println("estStream=" + estStream + ": 10 estimates=" + estStream.take(10).toList)
    finalVal(estStream)
  }

  def main(args: Array[String]) {
    val n: Int = if (args.length == 0) 9 else args(0).toInt
    println("First " + n + " primes numbers: " +  primeStream.take(n).toList)

    println("Square Root of n = " + fixedPoint(y=>(n/y)))
  }
}
