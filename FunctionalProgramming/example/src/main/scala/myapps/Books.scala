package myapps

import common._
import mystl.NLists._

object Books {
  case class Book(title: String, authors: NList[String])
  val books: NList[Book] = NList(
    Book("Structure and Interpretation of Computer Programs",
      NList("Abelson, Harold", "Sussman, Gerald J.")),
    Book("Principles of Compiler Design",
      NList("Aho, Alfred", "Ullman, Jeffrey")),
    Book("Programming in Modula-2",
      NList("Wirth, Niklaus")),
    Book("Introduction to Functional Programming",
      NList("Bird, Richard")),
    Book("Text Book",
      NList("Ullman, Jeffrey")),
    Book("The Java Language Specification",
      NList("Gosling, James", "Joy, Bill", "Steele, Guy", "Bracha, Gilad")))

  /*
   * Then, to find the titles of all books whose author's last name
   * is "Ullman":
   */
  def findBookTitlesOnAuthorLastName(blist: NList[Book], s: String): NList[String] = {
    /*
     * for (b <- blist; a <- b.authors if a startsWith "Ullman")
     * yield b.title
     */
    blist.filter(b => b.authors.exists(author => author startsWith s)).map(b => b.title)
  }
  /*
   * Find the titles of all books that have s in their title:
   */
  def findBookTitlesOnTitleSubstring(blist: NList[Book], s: String): NList[String] = {
    /*
     * for (b <- blist if (b.title indexOf s) >= 0)
     * yield b.title
     */
     
    blist.filter(b => b.title.contains(s)).map(b => b.title)
  }
  /*
   * Find the names of all authors that have written at least two books in the database.
   */
  def findAuthorsWithNBooks(blist: NList[Book], n: Int): NList[String] = {
     (for (b1 <- blist; b2 <- blist if b1 != b2;
       a1 <- b1.authors;
       a2 <- b2.authors if a1 == a2)
     yield a1).removeDuplicates
  }
  /*
   * Express the following function using ForComprehension
   */
  def flatten[T](xss: NList[NList[T]]): NList[T] = {
    /*
     * (xss :\ (Nil: NList[T])) ((xs, ys) => xs ::: ys)
     */
    for (xlist <- xss; x <- xlist) yield x
  }

  def main(args: Array[String]) {
    println("Query Output: ")
    println("All books authored by last name 'Ullman'")
    findBookTitlesOnAuthorLastName(books, "Ullman").foreach(x => println("  " + x))
    println("All books with 'Program' in its title")
    findBookTitlesOnTitleSubstring(books, "Program").foreach(x => println("  " + x))
    println("All Authors with two books")
    findAuthorsWithNBooks(books, 2).foreach(x => println("  " + x))
    print("Flatten: 'NList(NList(), NList(1), NList(2, 3), NList(4)' = ")
    println(flatten(NList(NList(), NList(1), NList(2, 3), NList(4))))
  }  
}
