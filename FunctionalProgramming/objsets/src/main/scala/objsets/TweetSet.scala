package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + ";" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet = filterAcc(x => true, that)

  /**
   * New method created to test when the mostRetweeted is executed
   * on Empty set: in that case just return a dummy Tweet with
   * min possible retweets - that tweet will be anyway killed in
   * mostRetweeted method
   */
  def mostRetweetedExists: Tweet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList

  val numElems: Int // debug

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  /*
   * Nothing more to filter: return already accumulated TweetSet "acc"
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * New method created to test when the mostRetweeted is executed
   * on Empty set: in that case just return a dummy Tweet with
   * min possible retweets - that tweet will be anyway killed in
   * mostRetweeted method
   */
  def mostRetweetedExists: Tweet = new Tweet("DummyUser", "DummyText", Int.MinValue)

  def mostRetweeted: Tweet = throw new NoSuchElementException("Empty.mostRetweeted")

  def descendingByRetweet: TweetList = Nil

  override def toString: String = "'nil'" // debug

  val numElems: Int = 0 // debug

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if (p(elem))
      (left.filterAcc(p, right.filterAcc(p, acc))).incl(elem)
    else
      left.filterAcc(p, right.filterAcc(p, acc))
  }

  /**
   * New method created to test when the mostRetweeted is executed
   * on Empty set: in that case just return a dummy Tweet with
   * min possible retweets - that tweet will be anyway killed in
   * mostRetweeted method
   */
  def mostRetweetedExists: Tweet = mostRetweeted

  def mostRetweeted: Tweet = {
    val leftMostRetweetedTweet = left.mostRetweetedExists
    val rightMostRetweetedTweet = right.mostRetweetedExists
    val mostRetweetNum = math.max(elem.retweets,
      math.max(leftMostRetweetedTweet.retweets, rightMostRetweetedTweet.retweets))

    if (elem.retweets == mostRetweetNum)
      elem
    else if (leftMostRetweetedTweet.retweets == mostRetweetNum)
      leftMostRetweetedTweet
    else
      rightMostRetweetedTweet
  }

  def descendingByRetweet: TweetList = {
    val mostRetweetedTwit = mostRetweeted
    val remainingTweetSet = remove(mostRetweeted)
    new Cons(mostRetweetedTwit, remainingTweetSet.descendingByRetweet)
  }

  override def toString: String = "{" + left + " '" + elem + "' " + right + "}" // debug
  val numElems = 1 + left.numElems + right.numElems

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
  override def toString = "."
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
  override def toString = head + "->" + tail
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val allTweets = TweetReader.allTweets
  lazy val googleTweets: TweetSet = 
    allTweets.filter(tweet => google.exists(keyword => tweet.text contains keyword))
  lazy val appleTweets: TweetSet = 
    allTweets.filter(tweet => apple.exists(keyword => tweet.text contains keyword))

  lazy val googleOrAppleTweets = googleTweets union appleTweets
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleOrAppleTweets.descendingByRetweet
}

object Main extends App {
  /*
   * // Print the trending tweets
   * GoogleVsApple.trending foreach println
   */
  val emp  = new Empty
  val u1t1 = emp.incl(new Tweet("u1", "t1", 8));
  val u1t2 = emp.incl(new Tweet("u1", "t2", 20));
  val u2t3 = emp.incl(new Tweet("u2", "t3", 4));
  val u3t4 = emp.incl(new Tweet("u3", "t4", 24));

  println("Empty: " + new Empty)
  println("u1t1: " + u1t1);
  println("u1t2: " + u1t2);
  println("u2t3: " + u2t3);
  println("u3t4: " + u3t4);

  val uofall = u1t1 union u1t2 union u2t3 union u3t4

  println("uofall =  u1t1 union u1t2 union u2t2 union u3t4 = " + uofall)
  println("# elems uofall = " + uofall.numElems)
  println("max retweet: " + uofall.mostRetweeted)
  println("sorted TweetList: " + uofall.descendingByRetweet)

  println("TweetReader.allTreets: numElems " + GoogleVsApple.allTweets.numElems)
  println("Most popular tweet overall: " + allTweets.mostRetweeted)

  println("Tweets with google keywords: " + GoogleVsApple.googleTweets.numElems)
  println("Tweets with apple keywords: " + GoogleVsApple.appleTweets.numElems)
  println("Tweets with google or apple keywords: " + GoogleVsApple.googleOrAppleTweets.numElems)

  println("Most popular tweet among Google/Apple: " + GoogleVsApple.trending.head)
}
