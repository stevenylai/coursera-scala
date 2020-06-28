package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  val maxMeldDepth = 1024
  def inserted(depth: Int): Gen[H] = for {
    i <- arbitrary[A]
    heap <- randomHeap(depth)
  } yield insert(i, heap)
  def melded(depth: Int): Gen[H] = for {
    left <- randomHeap(depth)
    right <- randomHeap(depth)
  } yield meld(left, right)
  def randomHeap(depth: Int): Gen[H] = if (depth < maxMeldDepth) oneOf (
    const(empty),
    Gen.lzy(inserted(depth)),
    Gen.lzy(melded(depth + 1))
  ) else oneOf(const(empty), Gen.lzy(inserted(depth)))
  def genHeap: Gen[H] = randomHeap(1)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  property("specific") = forAll { (i: Int) =>
    val increasing = insert(3, insert(2, insert(1, empty)))
    val decreasing = insert(1, insert(2, insert(3, empty)))
    val mixed = insert(1, insert(3, insert(2, empty)))
    findMin(increasing) == findMin(decreasing) && findMin(decreasing) == findMin(mixed) &&
      findMin(deleteMin(increasing)) == findMin(deleteMin(decreasing)) &&
      findMin(deleteMin(decreasing)) == findMin(deleteMin(mixed)) &&
      findMin(deleteMin(deleteMin(increasing))) == findMin(deleteMin(deleteMin(decreasing))) &&
      findMin(deleteMin(deleteMin(decreasing))) == findMin(deleteMin(deleteMin(mixed))) &&
      isEmpty(deleteMin(deleteMin(deleteMin(increasing)))) &&
      isEmpty(deleteMin(deleteMin(deleteMin(decreasing)))) &&
      isEmpty(deleteMin(deleteMin(deleteMin(mixed))))
  }
  property("min of 2 inserted into empty is min of 2") = forAll {(i: A, j: A) =>
    findMin(insert(j, insert(i, empty))) == Math.min(i, j)
  }
  property("insert one and delete from empty") = forAll { (i: A) =>
    val withOne = insert(i, empty)
    val removedOne = deleteMin(withOne)
    def findMinShouldThrow(h: H) = try {
        findMin(h)
        false
      } catch {
        case _: NoSuchElementException => true
        case _: Throwable => false
    }
    findMin(withOne) == i && isEmpty(removedOne) && findMinShouldThrow(removedOne)
  }
  property("min of melded is min of individual") = forAll { (h1: H, h2: H) =>
    if (!isEmpty(h1) && !isEmpty(h2)) {
      val min = Math.min(findMin(h1), findMin(h2))
      findMin(meld(h1, h2)) == min
    } else if (!isEmpty(h1)) {
      findMin(meld(h1, h2)) == findMin(h1)
    } else if (!isEmpty(h2)) {
      findMin(meld(h1, h2)) == findMin(h2)
    } else true
  }
  property("removed elements are sorted") = forAll { (h: H) =>
    def buildSorted(h: H, cur: List[A]): List[A] = {
      if (isEmpty(h)) cur else {
        val min = findMin(h)
        buildSorted(deleteMin(h), min :: cur)
      }
    }
    val sorted = buildSorted(h, Nil)
    sorted.reverse == sorted.sorted
  }
}
