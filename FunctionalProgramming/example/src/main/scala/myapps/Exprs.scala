package myapps

import common._

object Exprs {
  trait Expr {
    def eval: Int = this match {
      case Num(n) => n
      case Sum(left, right) => left.eval + right.eval
      case Pro(left, right) => left.eval * right.eval
    }
    def disp: String = this match {
      case Num(n) => n.toString
      case Sum(left, right) => "(" + left.disp + "+" + right.disp + ")"
      case Pro(left, right) => left.disp + "*" + right.disp
    }
  }
  case class Num(n: Int) extends Expr
  case class Sum(left: Expr, right: Expr) extends Expr
  case class Pro(left: Expr, right: Expr) extends Expr

  def main(args: Array[String]) {
    println("Expression Evaluation: ")
    println("Num(10) = " + Num(10).eval)
    println("Pro(Num(10), Sum(Num(5), Num(3))) = " +
      Pro(Num(10), Sum(Num(5), Num(3))).eval)
    println("Pro(Num(10), Sum(Num(5), Num(3))).disp = " +
      Pro(Num(10), Sum(Num(5), Num(3))).disp)
    println("Sum(Num(10), Pro(Num(5), Num(3))) = " +
      Sum(Num(10), Pro(Num(5), Num(3))).eval)
    println("Sum(Num(10), Pro(Num(5), Num(3))).disp = " +
      Sum(Num(10), Pro(Num(5), Num(3))).disp)
  }  
}
