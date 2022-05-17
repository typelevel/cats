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
import cats.kernel.laws.discipline.CommutativeMonoidTests
import cats.kernel.laws.discipline.HashTests
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.UnorderedFoldableTests
import cats.laws.discipline.SerializableTests
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scala.collection.mutable
import scala.util.Random

class HashSetSuite extends CatsSuite {

  checkAll("HashSet[Int]", HashTests[HashSet[Int]].hash)
  checkAll("Hash[HashSet[Int]]", SerializableTests.serializable(HashSet.catsDataHashForHashSet[Int]))

  checkAll("HashSet[Int]", UnorderedFoldableTests[HashSet].unorderedFoldable[Int, Int])
  checkAll("UnorderedFoldable[HashSet]", SerializableTests.serializable(HashSet.catsDataUnorderedFoldableForHashSet))

  checkAll("HashSet[String]", CommutativeMonoidTests[HashSet[String]].commutativeMonoid)
  checkAll("CommutativeMonoid[HashSet[String]]",
           SerializableTests.serializable(HashSet.catsDataCommutativeMonoidForHashSet[String])
  )

  // Based on https://stackoverflow.com/questions/9406775/why-does-string-hashcode-in-java-have-many-conflicts
  // We can produce a collision for any string by adding 1 to and subtracting 31 from two consecutive chars.
  def collidingString(str: String) = {
    if (str.length < 2)
      str
    else {
      val randomOffset = Random.nextInt(str.length - 1)
      val firstChar = str.substring(randomOffset).charAt(0)
      val secondChar = str.substring(randomOffset).charAt(1)

      if (!Character.isDefined(firstChar + 1) || !Character.isDefined(secondChar - 31))
        str
      else
        str
          .updated(randomOffset, (firstChar + 1).toChar)
          .updated(randomOffset + 1, (secondChar - 31).toChar)
    }
  }

  def genString(hs: HashSet[String]): Gen[String] =
    if (hs.isEmpty)
      arbitrary[String]
    else
      Gen.oneOf(
        Gen.oneOf(hs.iterator.toList).map(collidingString),
        arbitrary[String]
      )

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
    forAll { (hashSet: HashSet[Int]) =>
      assert(hashSet === hashSet)
    }

    forAll { (left: HashSet[Int], right: HashSet[Int]) =>
      val lefts = left.iterator.toList
      val rights = right.iterator.toList
      assert((lefts === rights) === (left === right))
    }
  }

  test("size") {
    assert(HashSet.empty[Int].size === 0)
    assert(HashSet(1, 2, 3).size === 3)
    assert(HashSet("Aa", "BB").size == 2)

    forAll { (hashSet: HashSet[Int]) =>
      assert(hashSet.iterator.toList.size === hashSet.size)
    }
  }

  test("union") {
    assert(HashSet.empty[Int].union(HashSet(1, 2, 3)) === HashSet(1, 2, 3))
    assert(HashSet(1, 2, 3).union(HashSet.empty[Int]) === HashSet(1, 2, 3))

    assert(HashSet(1, 2, 3).union(HashSet(1, 2, 3)) === HashSet(1, 2, 3))

    assert(HashSet(1, 2, 3).union(HashSet(4, 5, 6)) === HashSet(1, 2, 3, 4, 5, 6))

    assert(HashSet("Aa").union(HashSet("BB")) === HashSet("Aa", "BB"))
    assert(HashSet("Aa", "BB").union(HashSet("Aa", "BB", "Ca", "DB")) === HashSet("Aa", "BB", "Ca", "DB"))

    forAll { (left: HashSet[Int], right: HashSet[Int]) =>
      val both = left.union(right)
      val lefts = left.iterator.toList
      val rights = right.iterator.toList
      (lefts ++ rights).foreach { v =>
        assert(both.contains(v))
      }
    }
  }

  test("add") {
    forAll { (initial: HashSet[String]) =>
      forAll(genString(initial)) { (value: String) =>
        val updated = initial.add(value)
        assert(updated.contains(value))
        if (initial.contains(value))
          assert(updated.size === initial.size)
        else
          assert(updated.size === (initial.size + 1))
      }
    }
  }

  test("remove") {
    forAll { (initial: HashSet[String]) =>
      forAll(genString(initial)) { (value: String) =>
        val removed = initial.remove(value)
        assert(!removed.contains(value))
        if (initial.contains(value))
          assert(removed.size === (initial.size - 1))
        else
          assert(removed === initial)
      }
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

  property("fromIterableOnce consistent with contains") {
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromIterableOnce(ints.view)

      ints.foreach { i =>
        assert(hashSet.contains(i))
      }
    }
  }

  property("fromFoldable consistent with contains") {
    forAll { (ints: List[Int]) =>
      val hashSet = HashSet.fromFoldable(ints)

      ints.foreach { i =>
        assert(hashSet.contains(i))
      }
    }
  }

  property("iterator consistent with contains") {
    forAll { (hashSet: HashSet[Int]) =>
      val iterated = mutable.ListBuffer[Int]()
      hashSet.iterator.foreach { iterated += _ }
      assert(iterated.toList.distinct === iterated.toList)
      assert(iterated.forall(hashSet.contains))
    }
  }

  property("foreach consistent with contains") {
    forAll { (hashSet: HashSet[Int]) =>
      val foreached = mutable.ListBuffer[Int]()
      hashSet.foreach { foreached += _ }
      assert(foreached.toList.distinct === foreached.toList)
      assert(foreached.forall(hashSet.contains))
    }
  }

  property("foreach and iterator consistent") {
    forAll { (hashSet: HashSet[Int]) =>
      val iterated = mutable.ListBuffer[Int]()
      val foreached = mutable.ListBuffer[Int]()
      hashSet.iterator.foreach { iterated += _ }
      hashSet.foreach { foreached += _ }
      assert(foreached.forall(iterated.contains))
      assert(iterated.forall(foreached.contains))
    }
  }

  property("size consistent with iterator") {
    forAll { (hashSet: HashSet[Int]) =>
      var size = 0
      hashSet.iterator.foreach { _ => size += 1 }
      assert(hashSet.size === size)
    }
  }

  property("size consistent with foreach") {
    forAll { (hashSet: HashSet[Int]) =>
      var size = 0
      hashSet.foreach { _ => size += 1 }
      assert(hashSet.size === size)
    }
  }

  property("show consistent with ===") {
    forAll { (left: HashSet[Int], right: HashSet[Int]) =>
      if (left.show === right.show)
        assert(left === right)
    }
  }

  property("toSet consistent with fromIterableOnce") {
    forAll { (scalaSet: Set[Int]) =>
      val hashSet = HashSet.fromIterableOnce(scalaSet)
      val wrappedHashSet = hashSet.toSet
      assertEquals(scalaSet, wrappedHashSet)
    }
  }

  property("union consistent with Scala Set union") {
    forAll { (left: List[Int], right: List[Int]) =>
      val scalaSet = Set(left: _*) | Set(right: _*)
      val catsSet = HashSet.fromSeq(left).union(HashSet.fromSeq(right))
      assert(scalaSet.forall(catsSet.contains))
      catsSet.foreach(int => assert(scalaSet.contains(int)))
    }
  }
}
