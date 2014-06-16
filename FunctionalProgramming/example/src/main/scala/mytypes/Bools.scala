package mytypes

object Bools {
  /*
   * boolVal.ifThenElse(te, ee) may be interpreted as the following:
   * if (boolVal) then te else ee
   * where x is "te: then expression"
   * and y is "ee: else expression"
   */
  trait Bool {
    def ifThenElse[T](te: => T, ee: => T): T
    def unary_! = ifThenElse(FALSE, TRUE)
    def && (x: => Bool): Bool = ifThenElse(x, FALSE)
    def || (x: => Bool): Bool = ifThenElse(TRUE, x)
    def == (x: => Bool): Bool = ifThenElse(x, !x)
    def < (x: => Bool): Bool = ifThenElse(FALSE, x)
    def <= (x: => Bool): Bool = ( == (x) || < (x) )
    def > (x: => Bool): Bool = !( <= (x) ) 
    def >= (x: => Bool): Bool = !( < (x) )
    // toBoolean: written just to implement test cases
    def toBoolean: Boolean 
  }

  object FALSE extends Bool {
    def ifThenElse[T](te: => T, ee: => T): T = ee
    def toBoolean = false
    override def toString: String = "FALSE"
  }

  object TRUE extends Bool {
    def ifThenElse[T](te: => T, ee: => T): T = te
    def toBoolean = true
    override def toString: String = "TRUE"
  }

  def main(args: Array[String]) {
    println("Bool Operations")
    val f: Bool = FALSE
    val t: Bool = TRUE

    println("F " + f + ": T " + t)
    println("!f " + (!f) + ": !t " + (!t))
    println("f && t " + (f && t) + ": t && f " + (t && f))
    println("f || t " + (f || t) + ": t || f " + (t || f))
    println("f == t " + (f == t) + ": t == t " + (t || t))
    println("f < t " + (f < t) + ": t < f " + (t < f))
    println("f <= t " + (f <= t) + ": t <= f " + (t <= f))
    println("f > t " + (f > t) + ": t > f " + (t > f))
    println("f >= t " + (f >= t) + ": t >= f " + (t >= f))
    println("(f && t == FALSE).toBoolean " + ((f && t) == FALSE).toBoolean)
  }
}
