package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = 
    for
        element <- arbitrary[A]
        heap <- oneOf(Gen.const(empty), genHeap)
    yield insert(element, heap)
    
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("inserting two elements and checking min") = forAll { (elem1: A, elem2: A) =>
    val heap1 = insert(elem1, empty)
    val heap2 = insert(elem2, heap1)
    findMin(heap2) == elem1.min(elem2)
  }

  property("deleting a single element from a heap should return an empty heap") = forAll {
    (element: A) =>
      val heap1 = insert(element, empty)
      val heap2 = deleteMin(heap1)
      isEmpty(heap2)
  }

  property("deleting till empty should return sorted list") = forAll {
    (heap: H) =>
      def emptyAllElementsFromHeap(heep: H, acc: List[A]) : List[A] =
        if isEmpty(heep) then acc
        else emptyAllElementsFromHeap(deleteMin(heep), findMin(heep) :: acc)

      if isEmpty(heap) then true
      else
        val elementList = emptyAllElementsFromHeap(heap, Nil)
        elementList.reverse == elementList.sorted
  }

  property("min of melded heap should equal the min of one of the two original heaps") = forAll {
    (heap1: H, heap2: H) =>
      val meldedHeap = meld(heap1, heap2)
      val min1 = findMin(heap1)
      val min2 = findMin(heap2)

      findMin(meldedHeap) == min1.min(min2)
  }

  property("inserting 3 elements and deleting min twice should return the max element") = forAll {
    (elem1: A, elem2: A, elem3: A) =>
      val maxElem = (elem1.max(elem2)).max(elem3)
      val heap = insert(elem1, insert(elem2, insert(elem3, empty)))
      val resultHeap = deleteMin(deleteMin(heap))
      val resultElem = findMin(resultHeap)

      maxElem == resultElem
  }