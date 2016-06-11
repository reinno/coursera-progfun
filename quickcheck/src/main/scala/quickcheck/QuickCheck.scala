package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    m <- frequency((100, genHeap), (1, const(empty)))
  } yield insert(x, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (m1: A, m2: A) =>
    val m = if (m1 < m2) m1 else m2
    findMin(insert(m2, insert(m1, empty))) == m
  }

  property("gen3") = forAll { (m: A) =>
    isEmpty(deleteMin(insert(m, empty)))
  }

  property("gen4") = forAll { (h: H) =>
    def loop(h: H, last: A): Boolean = {
      if (isEmpty(h)) {
        true
      }
      else {
        val m  = findMin(h)
        if (m < last) false
        else loop(deleteMin(h), m)
      }
    }

    loop(deleteMin(h), findMin(h))
  }


  property("gen5") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
    val m = if (m1 < m2) m1 else m2
    //findMin(meld(h1, h2)) == m

    if (isEmpty(h1)) {
      if (isEmpty(h2)) {
        true
      } else {
        findMin(meld(h1, h2)) == m1
      }
    } else {
      if (isEmpty(h2)) {
        findMin(meld(h1, h2)) == m2
      } else {
        findMin(meld(h1, h2)) == m
      }
    }
  }


  property("gen6") = forAll { (x: List[A]) =>
    def getNum(h: H, num: Int): Int = {
      if (isEmpty(h)) {
        num
      }
      else {
        getNum(deleteMin(h), num + 1)
      }
    }

    def loopInsert(h: H, x: List[A]): H = {
      x match {
        case Nil => h
        case head::tail => loopInsert(insert(head, h), tail)
      }
    }

    def loop(h: H, last: A): Boolean = {
      if (isEmpty(h)) {
        true
      }
      else {
        val m  = findMin(h)
        if (m < last) false
        else loop(deleteMin(h), m)
      }
    }

    val h = loopInsert(empty, x)
    getNum(h, 0) == x.size && (if (isEmpty(h)) true else loop(deleteMin(h), findMin(h)))
  }
}
