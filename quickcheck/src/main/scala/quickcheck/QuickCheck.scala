package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.mutable.ListBuffer

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    element <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(element, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    a == findMin(h)
  }

  property("min2") = forAll { a: Int =>
    val h = insert(a, empty)
    val hdel = deleteMin(h)
    isEmpty(hdel)
  }

  property("minIns") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val min = findMin(h)
    if(a < b)
      min == a
    else
      min == b
  }

  property("mindel") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val newh = deleteMin(h)
    if(a < b)
      findMin(newh) == b
    else
      findMin(newh) == a
  }

  property("minIns2") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, insert(b, insert(c, empty)))
    val min = findMin(h)
    if(a < min || b < min || c < min) false
    else true
  }

  property("heapSort") = forAll{ heap: H =>
    def extractMins(h: H): List[Int] = {
      if(isEmpty(h)) List()
      else
        findMin(h) :: extractMins(deleteMin(h))
    }

    def isOrdered(l: List[Int]): Boolean = {
      if(l.isEmpty) true
      else if(l.tail.isEmpty) true
      else l.head <= l.tail.head && isOrdered(l.tail)
    }

    val list = extractMins(heap)
    isOrdered(list.reverse)
  }

  property("minMeld") = forAll{ (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val h3 = meld(h1, h2)
    val min3 = findMin(h3)
    min1 == min3 || min3 == min3
  }

  property("minMeldEmpty") = forAll{ h1: H =>
    val h2 = empty
    val min1 = findMin(h1)
    val h3 = meld(h1, h2)
    val min3 = findMin(h3)
    min1 == min3
  }

  property("intmin") = forAll { h: H =>
    val min = findMin(h)
    val delh = deleteMin(h)
    val newmin = Int.MinValue
    val hnewmin = insert(min, delh)
    findMin(hnewmin) == min
  }

  property("newmin") = forAll { h: H =>
    val min = findMin(h)
    val delh = deleteMin(h)
    if(Int.MinValue < min) {
      val newmin = min - 1
      val hnewmin = insert(newmin, delh)
      findMin(hnewmin) == newmin
    } else {
      val newmin = Int.MinValue
      val hnewmin = insert(newmin, delh)
      findMin(hnewmin) == newmin
    }
  }

  property("changemin") = forAll { (h: H, a: Int) =>
    val min = findMin(h)
    val newh = insert(Int.MinValue, h)
    val newh2 = insert(a, newh)
    val delh = deleteMin(newh2)
    findMin(delh) == min || findMin(delh) == a
  }
}
