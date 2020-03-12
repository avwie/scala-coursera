package objsets

import TweetReader._

/**
  * A class to represent tweets.
  */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

/**
  * This represents a set of objects of type `Tweet` in the form of a binary search
  * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
  * invariant which always holds: for every branch `b`, all elements in the left
  * subtree are smaller than the tweet at `b`. The elements in the right subtree are
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
abstract class TweetSet extends TweetSetInterface {
  def filter(p: Tweet => Boolean): TweetSet
  def mostRetweetedAcc(max: Tweet): Tweet = max
}

class Empty extends TweetSet {
  def filter(p: Tweet => Boolean): TweetSet = new Empty()

  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException()

  def contains(tweet: Tweet): Boolean = false

  def union(that: TweetSet): TweetSet = that

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  def descendingByRetweet: TweetList = Nil
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filter(p: Tweet => Boolean): TweetSet =
    if (p(elem)) new NonEmpty(elem, left.filter(p), right.filter(p))
    else left.filter(p) union right.filter(p)

  def union(that: TweetSet): TweetSet = left.union(right.union(that.incl(elem)))

  def mostRetweeted: Tweet = mostRetweetedAcc(elem)

  override def mostRetweetedAcc(max: Tweet): Tweet = {
    left.mostRetweetedAcc(
      right.mostRetweetedAcc(if (elem.retweets > max.retweets) elem else max)
    )
  }

  def descendingByRetweet: TweetList =
    new Cons(mostRetweeted, remove(mostRetweeted).descendingByRetweet)

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
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(tweet =>
    google.exists(keyword => tweet.text.contains(keyword))
  )
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(tweet =>
    apple.exists(keyword => tweet.text.contains(keyword))
  )

  lazy val trending: TweetList =
    (googleTweets union appleTweets).descendingByRetweet
}

object Main extends App {
  GoogleVsApple.trending foreach println
}
