package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
    val h2 = deleteMin(h)
    h2 == empty
  }

  property("min of two ints") = forAll { (a: Int, b: Int) =>
    val min = Math.min(a,b)
    val h = insert(b, insert(a, empty))
    findMin(h) == min
  }

  property("add") = forAll { (a: Int, h: H) =>
    val min = Math.min(a, findMin(h))
    val h2 = insert(a, h)
    findMin(h2) == min
  }

  property("min of two melded heaps") = forAll { (h1: H, h2: H )=>
    val h1min = findMin(h1)
    val h2min = findMin(h2)
    val h1h2 = meld(h2,h2)
    val h1h2min = findMin(h1h2)
    println(s"h1min=$h1min, h2min=$h2min, h1h2min=$h1h2min")
    if (h1h2min != Math.min(h1min, h2min)) {
      println(h1h2)
      println(s"$h1h2min != ${Math.min(h1min, h2min)}")
    }
    findMin(h1h2) == Math.min(h1min, h2min)
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- Gen.oneOf(const(empty), genHeap)
  } yield insert(k,m)

//  lazy val genHeap: Gen[H] = for {
//    k <- arbitrary[Int]
//    m <- Gen.oneOf(empty, genHeap)
//  } yield {m match {
//      case Nil: m
//      case h:H => insert(k,h)
//    }
//  }
//
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
