package mytypes

// Peano Numbers
object Nats {
  trait Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def + (that: Nat): Nat
    def - (that: Nat): Nat
    def == (that: Nat): Boolean
    def != (that: Nat): Boolean = !(this == that)
    def > (that: Nat): Boolean
    def >= (that: Nat): Boolean = ((this > that) || (this == that))
    def < (that: Nat): Boolean = !(this >= that)
    def <= (that: Nat): Boolean = !(this > that)
  }

  object Zero extends Nat {
    def isZero: Boolean = true
    def predecessor: Nat =
      throw new IndexOutOfBoundsException("predecessor of Zero")
    def successor: Nat = new Succ(Zero)
    def + (that: Nat): Nat = that
    def - (that: Nat): Nat =
      if (that == Zero) this
      else throw new IndexOutOfBoundsException("Zero - NonZero")
    def == (that: Nat): Boolean = that.isZero
    def > (that: Nat): Boolean = false

    override def toString = "0"
  }

  class Succ(n: Nat) extends Nat {
    def isZero: Boolean = false
    def predecessor: Nat = n
    def successor: Nat = new Succ(this)
    def + (that: Nat): Nat = predecessor + that.successor
    def - (that: Nat): Nat =
      if (that == Zero) this
      else predecessor - that.predecessor
    def == (that: Nat): Boolean =
      if (that == Zero) false
      else (predecessor == that.predecessor)
    def > (that: Nat): Boolean =
      if (that == Zero) true
      else (this.predecessor > that.predecessor)

    override def toString = "Succ(" + predecessor.toString + ")" 
  }

  def main(args: Array[String]) {
    println("Nat Operations")

    val nZero = Zero
    val nOne = new Succ(Zero)
    val nTwo = new Succ(nOne)
    val nFive = nTwo + nTwo + nOne

    println("nZero: " + nZero)
    println("nOne + nZero: " + (nOne + nZero))
    println("nZero + nOne: " + (nZero + nOne))
    println("nTwo: " + nTwo)
    println("nFive: " + nFive)
    println("nZero == nZero: " + (nZero == nZero))
    println("nOne - nZero: " + (nOne - nZero))
    println("nTwo - nZero: " + (nTwo - nZero))
    println("nTwo > nOne: " + (nTwo > nOne))
    println("nFive + nTwo: " + (nFive + nTwo))
    println("nFive - nTwo: " + (nFive - nTwo))
    println("nFive == nTwo: " + (nFive == nTwo))
    println("nFive != nTwo: " + (nFive != nTwo))
    println("nFive > nTwo: " + (nFive > nTwo))
    println("nFive >= nTwo: " + (nFive >= nTwo))
    println("nFive < nTwo: " + (nFive < nTwo))
    println("nFive <= nTwo: " + (nFive <= nTwo))
  }
}
