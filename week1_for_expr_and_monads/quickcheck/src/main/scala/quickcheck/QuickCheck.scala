package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for
      a <- arbitrary[A]
      m <- oneOf(const(empty), genHeap)
    yield insert(a, m)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("adding a single element to an empty heap, and then removing this element, should yield the element in question") =
    forAll { (a: Int) =>
      val h = insert(a, empty)
      findMin(h) == a
    }

  property("adding the minimal element, and then finding it, should return the element in question") =
    forAll { (h: H) =>
      val m = if isEmpty(h) then 0 else findMin(h)
      findMin(insert(m, h)) == m
    }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") =
    forAll { (a: Int, b: Int) =>
      val m = insert(a, insert(b, empty))
      findMin(m) == ord.min(a, b)
    }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") =
    forAll { (a: Int) =>
      isEmpty(deleteMin(insert(a, empty)))
    }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.") =
    forAll { (h: H) =>
      @tailrec
      def helper(h: H, prev: Int): Boolean =
        h match
          case h: H if isEmpty(h) => true
          case h: H =>
            val curr = findMin(h)
            prev <= curr && helper(deleteMin(h), curr)
      helper(h, Integer.MIN_VALUE)
    }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") =
    forAll { (h1: H, h2: H) =>
      val m = meld(h1, h2)
  //    findMin(m) == ord.min(findMin(h1), findMin(h2))
      (h1, h2) match
        case (h1, h2) if !isEmpty(h1) && !isEmpty(h2) =>
          findMin(m) == ord.min(findMin(h1), findMin(h2))
        case (h1, h2) if isEmpty(h1) && !isEmpty(h2) =>
          findMin(m) == findMin(h2)
        case (h1, h2) if !isEmpty(h1) && isEmpty(h2) =>
          findMin(m) == findMin(h1)
        case (h1, h2) if isEmpty(h1) && isEmpty(h2) =>
          isEmpty(m)
    }

  property("Insert three elements to an empty heap, then deleteMin twice from the heap, findMin should equal to the max of the three") =
    forAll { (a: A, b:A, c:A) =>
      val maxAll = List(a, b, c).max
      val h = insert(c, insert(b, insert(a, empty)))
      val hLast = deleteMin(deleteMin(h))
      findMin(hLast) == maxAll
    }