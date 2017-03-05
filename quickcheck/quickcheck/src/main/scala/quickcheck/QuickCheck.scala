package quickcheck

import java.lang.Math._

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    b <- oneOf(genHeap, Gen.const(empty))
  } yield insert(a, b)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert two find min should return min of two") = forAll { (first: A, second: A) =>
    val heapWithTwoElements = insert(second, insert(first, empty))
    findMin(heapWithTwoElements) == min(first, second)
  }

  property("insert empty into empty and delete should be empty") = forAll { (in: A) =>
    isEmpty(deleteMin(insert(in, empty)))
  }

  property("deleting minimum should always return sorted sequence") = forAll { (in: H) =>

    def checkIfSorted(someHeap: H): Boolean = {
      if (isEmpty(someHeap)) true
      else {
        val minimum = findMin(someHeap)
        deleteMin(someHeap)
        isEmpty(someHeap) || minimum <= findMin(someHeap)
      }
    }

    checkIfSorted(in)
  }

  property("minimum of melded should be minimum of one of heaps") = forAll { (one: H, two: H) =>
    isEmpty(one) || isEmpty(two) || findMin(meld(one, two)) == findMin(one) || findMin(meld(one, two)) == findMin(two)
  }

  property("min of one element should return element") = forAll { (in: A) =>
    findMin(insert(in, empty)) == in
  }

  property("min of two element heap should not be head") = forAll { (in: H) =>
    if (!isEmpty(in)) {
      val oldMin = findMin(in)
      findMin(insert(oldMin, in)) == oldMin
    } else true
  }

  property("the same heap melded two times should have the same min") = forAll {
    (o: H) =>
      isEmpty(o) || findMin(meld(o, o)) == findMin(o)
  }

  property("min of sorted elements inserted in empty is also heap min") = forAll {
    (a: A, b: A, c: A) =>
      val sorted = List(a, b, c).sorted
      val a1 = insert(sorted.head, empty)
      val a2 = insert(sorted(1), a1)
      val a3 = insert(sorted(2), a2)
      findMin(a3) == sorted.last
  }

  property("adding list of elements where any element is bigger than heap min should not change heap min") = forAll {
    (h: H, a: A, b: A, c: A) =>
      val firstMin = findMin(h) + 1
      val sorted = List(a, b, c).sorted.map(_ + firstMin)
      isEmpty(h) || findMin(insert(sorted.head, insert(sorted(1), insert(sorted(2), h)))) == firstMin
  }

  property("is empty") = forAll {
    (a: Int) =>
      isEmpty(empty)
  }

  property("deleting a min of two element heap") = forAll {
    (a: A, b: A) =>
      deleteMin(insert(a, insert(b, empty))) == insert(max(a, b), empty)
  }
}
