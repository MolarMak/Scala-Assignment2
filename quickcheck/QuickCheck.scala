package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- Arbitrary.arbitrary[List[Int]]
  } yield n.foldRight[H](empty)((next: Int, acc: H) => insert(next, acc))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back") = forAll {
    (h1: Int, h2: Int) =>
      val heap1 = insert(h1, empty)
      val heap2 = insert(h2, heap1)
      findMin(heap2) == List(h1, h2).min
  }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty") = forAll {
    h1: Int => deleteMin(insert(h1, empty)) == empty
  }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other") = forAll {
    (h1: H, h2: H) =>
      val meldHeap = meld(h1, h2)
      findMin(meldHeap) == findMin(h1) || findMin(meldHeap) == findMin(h2)
  }

  property("If you meld heap with empty, the resulting heap should be same") = forAll {
    h1: H => meld(h1, empty) == h1
  }

  property("If you meld two emptyes, the result heal should be empty") = forAll {
    _: Int => isEmpty(meld(empty, empty))
  }

}
