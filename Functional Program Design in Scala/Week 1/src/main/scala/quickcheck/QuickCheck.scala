package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("1") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    if (a < b) {
      a == findMin(h)
    } else {
      b == findMin(h)
    }
  }

  property("2") = forAll { a: Int => empty == deleteMin(insert(a, empty)) }

  property("3") = forAll { h: H =>
    def isSorted(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)

        if (!isEmpty(h2) && m > findMin(h2)) false
        else isSorted(h2)
      }
    }
    isSorted(h)
  }

  property("4") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)

    val h3 = meld(h1, h2)

    if (isEmpty(h3)) true
    else {
      val m3 = findMin(h3)
      m1 == m3 || m2 == m3
    }
  }

  property("5") = forAll { (h1: H, h2: H) =>
    def equal(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && equal(deleteMin(h1), deleteMin(h2))
      }
    equal(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
