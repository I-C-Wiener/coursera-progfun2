package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    isEmpty <- arbitrary[Boolean]
    h <- oneOf(const(empty), genNonEmptyHeap)
  } yield h

  lazy val genNonEmptyHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- genHeap
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minimum from two inserted into empty heap") = forAll { (a: A, b: A)=>
    val h = insert(b, insert(a, empty))
    val min = if ((a compareTo b) < 0) a else b
    min == findMin(h)
  }

  property("delete minimum from heap with one element") = forAll { a: A =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("always sorted after deleting minimal") = forAll { h: H =>
    isEmpty(h) || deleteMinFromHeap(findMin(h), h)
  }

  def deleteMinFromHeap(prevMin: A, h: H): Boolean = {
    isEmpty(h) || (prevMin <= findMin(h) && deleteMinFromHeap(findMin(h), deleteMin(h)))
  }

  property("minimum after meld equals to minimum of some heap") = forAll { (h1: H, h2: H) =>
    meldLoop(h1, h2, meld(h1, h2))
  }

  def meldLoop(h1: H, h2: H, m: H): Boolean =
    (m, h1, h2) match {
      case (List(), List(), List()) => true
      case (List(), _, t::ts) => false
      case (List(), t::ts, _) => false
      case (t::ts, List(), List()) => false
      case (_, List(), _) => findMin(h2) == findMin(m) && meldLoop(h1, deleteMin(h2), deleteMin(m))
      case (_, _, List()) => findMin(h1) == findMin(m) && meldLoop(deleteMin(h1), h2, deleteMin(m))
      case _ if findMin(h1) < findMin(h2) => meldLoop(deleteMin(h1), h2, deleteMin(m))
      case _ => meldLoop(h1, deleteMin(h2), deleteMin(m))
    }

}
