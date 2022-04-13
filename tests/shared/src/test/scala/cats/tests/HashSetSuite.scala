/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats.data.HashSet
import cats.syntax.eq._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scala.collection.mutable

class HashSetSuite extends CatsSuite {
  // Examples from https://stackoverflow.com/questions/9406775/why-does-string-hashcode-in-java-have-many-conflicts
  val collisions = for {
    left <- ('A' to 'Y').toList
    first = List(left, 'a').mkString
    second = List((left + 1).toChar, 'B').mkString
  } yield (first, second)

  val colliding = Gen.listOf(Gen.oneOf(collisions))

  test("show") {
    assert(HashSet(1, 2, 3).show === "HashSet(1, 2, 3)")
    assert(HashSet.empty[Int].show === "HashSet()")
  }

  test("isEmpty and nonEmpty") {
    assert(HashSet.empty[Int].isEmpty)
    assert(HashSet.empty[Int].show === "HashSet()")
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromSeq(ints)
      assert(hashSet.isEmpty === ints.isEmpty)
      assert(hashSet.nonEmpty === ints.nonEmpty)
    }
  }

  test("===") {
    forAll { (ints: List[Int]) =>
      val left = HashSet.fromSeq(ints)
      val right = HashSet.fromSeq(ints)
      assert(left === right)
    }

    forAll { (leftInts: List[Int], rightInts: List[Int]) =>
      val left = HashSet.fromSeq(leftInts)
      val right = HashSet.fromSeq(rightInts)
      assert((leftInts.distinct === rightInts.distinct) === (left === right))
    }
  }

  test("size") {
    assert(HashSet.empty[Int].size === 0)
    assert(HashSet(1, 2, 3).size === 3)
    assert(HashSet("Aa", "BB").size == 2)

    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromSeq(ints)
      assert(hashSet.size === ints.distinct.size)
    }
  }

  property("Empty HashSet never contains") {
    forAll { (i: Int) =>
      assert(!HashSet.empty[Int].contains(i))
    }
  }

  property("Empty HashSet add consistent with contains") {
    forAll { (i: Int) =>
      assert(HashSet.empty[Int].add(i).contains(i))
    }
  }

  property("Empty HashSet add and remove consistent with contains") {
    forAll { (i: Int) =>
      assert(!HashSet.empty[Int].add(i).remove(i).contains(i))
    }
  }

  property("fromSeq consistent with contains") {
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromSeq(ints)

      ints.foreach { i =>
        assert(hashSet.contains(i))
      }
    }
  }

  property("add with collisions consistent with contains") {
    forAll(colliding) { (collisions: List[(String, String)]) =>
      val hashSet = collisions.foldLeft(HashSet.empty[String]) { case (hs, (l, r)) =>
        hs.add(l).add(r)
      }

      collisions.foreach { case (l, r) =>
        assert(hashSet.contains(l))
        assert(hashSet.contains(r))

        val removeL = hashSet.remove(l)
        assert(removeL.contains(r))
        assert(!removeL.contains(l))

        val removeR = hashSet.remove(r)
        assert(removeR.contains(l))
        assert(!removeR.contains(r))

        val removeLR = removeL.remove(r)
        assert(!removeLR.contains(l))
        assert(!removeLR.contains(r))
      }
    }
  }

  property("remove consistent with contains") {
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromSeq(ints)

      ints.foreach { i =>
        assert(hashSet.contains(i))
        assert(!hashSet.remove(i).contains(i))
      }
    }
  }

  property("remove with collisions consistent with contains") {
    forAll(colliding) { (strings: List[(String, String)]) =>
      val hashSet = strings.foldLeft(HashSet.empty[String]) { case (hs, (l, r)) =>
        hs.add(l).add(r)
      }

      strings.foreach { case (l, r) =>
        assert(hashSet.contains(l))
        assert(hashSet.contains(r))

        val removeL = hashSet.remove(l)
        assert(removeL.contains(r))
        assert(!removeL.contains(l))

        val removeR = hashSet.remove(r)
        assert(removeR.contains(l))
        assert(!removeR.contains(r))

        val removeLR = removeL.remove(r)
        assert(!removeLR.contains(l))
        assert(!removeLR.contains(r))
      }
    }
  }

  property("iterator consistent with contains") {
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromSeq(ints)

      val iterated = mutable.ListBuffer[Int]()
      hashSet.iterator.foreach { iterated += _ }

      assert(ints.forall(iterated.contains))
      assert(iterated.forall(ints.contains))
      assert(iterated.distinct.toList === iterated.toList)
      assert(iterated.forall(hashSet.contains))
    }
  }

  property("iterator consistent with reverseIterator") {
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromSeq(ints)

      val iterated = mutable.ListBuffer[Int]()
      val reverseIterated = mutable.ListBuffer[Int]()
      hashSet.iterator.foreach { iterated += _ }
      hashSet.reverseIterator.foreach { reverseIterated += _ }

      assert(iterated.toList === reverseIterated.toList.reverse)
    }
  }

  property("foreach consistent with contains") {
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromSeq(ints)

      val foreached = mutable.ListBuffer[Int]()
      hashSet.foreach { foreached += _ }

      assert(ints.forall(foreached.contains))
      assert(foreached.forall(ints.contains))
      assert(foreached.distinct.toList === foreached.toList)
      assert(foreached.forall(hashSet.contains))
    }
  }

  property("foreach and iterator consistent") {
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromSeq(ints)

      val iterated = mutable.ListBuffer[Int]()
      val foreached = mutable.ListBuffer[Int]()
      hashSet.iterator.foreach { iterated += _ }
      hashSet.foreach { foreached += _ }

      assert(foreached.forall(iterated.contains))
      assert(iterated.forall(foreached.contains))
    }
  }

  property("size consistent with iterator") {
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromSeq(ints)

      var size = 0
      hashSet.iterator.foreach { _ => size += 1 }

      assert(hashSet.size === size)
    }
  }

  property("size consistent with foreach") {
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromSeq(ints)

      var size = 0
      hashSet.foreach { _ => size += 1 }

      assert(hashSet.size === size)
    }
  }
}
