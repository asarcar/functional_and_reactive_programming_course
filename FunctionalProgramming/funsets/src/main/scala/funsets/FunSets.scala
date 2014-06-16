package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = x => (x == elem)

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = x => (contains(s, x) || contains(t, x))

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = x => (contains(s, x) && contains(t, x))

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = x => (contains(s, x) && !contains(t, x))

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = x => (contains(s, x) && p(x))

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      /* We have reached the end of iteration: all elements in s must have
       * passed the predicate test: return true.
       */
      if (a > bound) true
      /* a is an element of s AND predicate test failed: return false */
      else if (contains(s, a) && !p(a)) false
      /* a was not in s OR predicate test passed for a:
       * continue looking for remaining candidates that might fail
       */
      else iter(a+1) 
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`:
   * s = (e1 U e2 U e3 ....)
   * exists <=> (p(e1) || p(e2) || p(e3) ...)
   * <=> !(!p(e1) && !p(e2) && ...) <=> !(for_all(s, !p))
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   * One way to interpret the above requirement: "x is an element of mapped
   * set if there exists an element i in s s.t. f(i) == n 
   */
  def map(s: Set, f: Int => Int): Set = x => exists(s, y => (f(y) == x))

  /**
    * Returns whether two sets are exactly same
    */
  def same(s1: Set, s2: Set): Boolean =
    forall(s1, x => contains(s2, x)) && forall(s2, x => contains(s1, x))

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
