package mystl

import common._

object BSets {
  /*
   * +T <% Ordered[T]:
   *   +T: Allows all covariant classes of T to be a subtype of the
   *   aggregate class. The Ordered[T]: Restricts the legal types that can be substituted
   *   for type T to only those types that be compared with T using 
   *   < and > operators.
   * 
   * T <: Ordered[T] 
   *   Ordered[T]: a trait that implements the comparison operators: <, >, ==, etc.
   *   Ideally allow all subtype classes that are subtype of
   *   AnyVal should have implemented the comparison operator traits.
   *   Unfortunately, due to legacy reasons (compatibility with Java native types?),
   *   this could not implemented.
   * 
   * T <% Ordered[T]:
   *   Allows T as subtype of all types that can be "implicitly converted to a type" which
   *   in turn allows comparison operations.
   */ 
  abstract class BSetP[+T <% Ordered[T]] {
    def isEmpty: Boolean
    def numElems: Int
    /*
     * U >: T <% Ordered[U]
     *   fn[U >: T](arg: U): res[U]
     *   1. Subtyping of functions are allowed only when the parameters are
     *      contravariant to the parent class and covariant to the child class.
     *   2. This ensures that parameter arguments are lower bounded by T i.e. arg U
     *      is always contravariant.
     *   3. The result should be covariant to T (i.e. W <: T).
     *      However, we are allowed to "bend" around that rule as all the methods
     *      in the class are immutable i.e. state is not modified, rather
     *      a new class is generated
     *
     * Child Method:    val c: (AnyRef => Int) = x => x.hashCode()
     * Parent Method:   val p: (String => AnyVal) = f
     *   1. Parameter: AnyRef is contravariant to String
     *   2. Result: Int is covariant to AnyVal
     *   Hence c can be a subtype of p: c <: p
     * 
     *   Now any function that was executed based on parent signature can be
     *   easily executed seamlessly without causing any issues:
     *   a. var v: AnyVal = p("abc") // valid: Int returned is a subtype of AnyVal
     *   b. v.mutate() // valid: mutate() on AnyVal is valid on Int (a subtype)
     * 
     */
    def contains[U >: T <% Ordered[U]](oElem: U): Boolean = false
    def incl[U >: T <% Ordered[U]](nElem: U): BSetP[U] =
      new BSet[U](nElem, NullBSet, NullBSet)
    def union[U >: T <% Ordered[U]](other: BSetP[U]): BSetP[U] = other
  }

  object NullBSet extends BSetP[Nothing] {
    def isEmpty = true
    def numElems = 0
    override def toString = "."
  }

  class BSet[+T <% Ordered[T]](e: T, l: BSetP[T], r: BSetP[T]) extends BSetP[T] {
    val elem: T = e
    val left: BSetP[T] = l
    val right: BSetP[T] = r
    def isEmpty = false
    def numElems = left.numElems + 1 + right.numElems
    override def contains[U >: T <% Ordered[U]](oElem: U) = {
      if (oElem < elem) {
        left.contains(oElem)
      } else if (oElem > elem) {
        right.contains(oElem)
      } else {
        true
      }
    }
    override def incl[U >: T <% Ordered[U]](nElem: U) = {
      if (nElem < elem) {
        new BSet[U](elem, left.incl(nElem), right)
      } else if (nElem > elem) {
        new BSet[U](elem, left, right.incl(nElem))
      } else {
        // else element already in list return the original list
        this
      }
    }
    override def union[U >: T <% Ordered[U]](other: BSetP[U]) = {
      if (other.isEmpty) {
        this
      } else {
        ((left union right) union other).incl(elem)
      }
    }
    override def toString = "{" + left.toString + elem + right.toString + "}"
  }

  def main(args: Array[String]) {
    println("----------------")

    val nullSet = NullBSet

    val x1 = nullSet.incl(8) // {8}
    val x2 = nullSet.incl(4).incl(21).incl(6) // {4 21 6}
    val x3 = nullSet.incl(5).incl(20).incl(3) // {5 20 3}
    val x4 = x2 union x3

    println("x1: " + x1 + ": x2: " + x2 + ": x3: " + x3 + ": x4: " + x4)

    println("----------------")
  }
}
