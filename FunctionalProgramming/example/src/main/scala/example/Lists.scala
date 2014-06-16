package example

import common._

object Lists {
  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def sum(xs: List[Int]): Int = {
    def sum_head_and_rest(head: Int, rest: List[Int]): Int = {
      if (rest.isEmpty)
        head
      else
        head + sum(rest)
    }
    /*
     * Artificially writing bad code to pass the exception throw for empty list
     * if (xs.isEmpty)
     *  0
     * else
     *  sum_head_vs_rest(xs.head, xs.tail);
     */
    sum_head_and_rest(xs.head, xs.tail)
  } 

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
   def max(xs: List[Int]): Int = {
     def max_head_vs_rest(head: Int, rest: List[Int]): Int = {
       if (rest.isEmpty)
         head
       else
         scala.math.max(head, max(rest))
     }
     /*
      * Artificially writing bad code to pass the exception throw for empty list
      * if (xs.isEmpty)
      *  scala.Int.MinValue
      * else
      *  max_head_vs_rest(xs.head, xs.tail);
      */
     max_head_vs_rest(xs.head, xs.tail);
   }
}
