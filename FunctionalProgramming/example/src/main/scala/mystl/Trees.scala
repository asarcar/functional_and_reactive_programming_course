package mystl

import common._

object Trees {
  abstract class Tree[+T <% Ordered[T]] {
    def length: Int = this match {
      case EmptyTree() => 0
      case Node(elem, left, right) => 1 + left.length + right.length
    }
    def contains[U >: T <% Ordered[U]](v: U): Boolean = this match {
      case EmptyTree() => false
      case Node(elem, left, right) => {
        if (v < elem)
          left.contains(v)
        else if (v > elem)
          right.contains(v)
        else
          true
      }
    }
    def insert[U >: T <% Ordered[U]](v: U): Tree[U] = this match {
      case EmptyTree() => Node(v, EmptyTree[T](), EmptyTree[T]())
      case Node(elem, left, right) => {
        if (v < elem)
          Node(elem, left.insert(v), right)
        else if (v > elem)
          Node(elem, left, right.insert(v))
        else
          this
      }
    }
  }

  case class EmptyTree[+T <% Ordered[T]]() extends Tree[T]
  case class Node[+T <% Ordered[T]](elem: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  def main(args: Array[String]) {
    println("----------------")

    // Basic
    val x0: Tree[Int] = EmptyTree()
    println("x0: " + x0)
    val x1: Tree[Int] = Node(10, x0, x0)
    println("x1: " + x1)

    // Contains
    println("x0 contains 10: " + x0.contains(10) + "; x1 contains 10: " + x1.contains(10))

    // Insert
    val x2 = x1.insert(5).insert(20)
    println("x1.insert(5): " + x1.insert(5) + "; x1.insert(5).insert(20): " + x2)

    println("----------------")
  }
}
