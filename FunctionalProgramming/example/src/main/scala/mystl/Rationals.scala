package mystl

import common._

object Rationals {
  class Rational(x: Int , y: Int) {
    require(y != 0, "Rational number creation with denominator Int >= 1")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = {
      if (b == 0)
        a
      else
        gcd(b, a%b)
    }

    private val g: Int   = gcd(x, y)
    private val num: Int = x/g
    private val den: Int = y/g

    private def isZero: Boolean = (x == 0)
    def + (o: Rational): Rational = new Rational(num*o.den + den*o.num, den*o.den)
    def unary_- = new Rational(-num, den)
    def - (o: Rational): Rational = this + -o
    def * (o: Rational): Rational = new Rational(num*o.num, den*o.den)
    def / (o: Rational): Rational = new Rational(num*o.den, den*o.num)
    def == (o: Rational): Boolean = (this + -o).isZero
    override def toString = {
      if ((den == 1) || (den == -1))
        (den*num).toString
      else if (den < 0)
        -num + "/" + -den
      else
        num + "/" + den
    }
  }

  def main(args: Array[String]) {
    val rone = new Rational(1);
    val r1 = new Rational(2, 3)
    val r2 = new Rational(3, 4)

    println("'2/3' = " + r1)
    println("'-3/4' = " + (-r2))
    println("'2/3 - 3/4' = " + (r1 - r2))
    println("'2/3*3/4' = " + (r1*r2))
    println("'1/(2/3)' = " + (rone/r1))
    println("'(2/3)/(3/4)' = " + (r1/r2))

    println("'(2/3 + 3/4 - (2/3*3/4))/((3/4)-(2/3))' = " + ((r1 + r2 - r1*r2)/(r2 - r1)))
    println("----------------")
  }
}
