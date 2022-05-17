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

import cats.data.HashMap
import cats.kernel.laws.discipline.CommutativeMonoidTests
import cats.kernel.laws.discipline.HashTests
import cats.laws.discipline.SerializableTests
import cats.laws.discipline.UnorderedTraverseTests
import cats.laws.discipline.arbitrary._
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.collection.mutable
import scala.util.Random

class HashMapSuite extends CatsSuite {
  checkAll("HashMap[Int, String]", HashTests[HashMap[Int, String]].hash)
  checkAll("Hash[HashMap[Int, String]]", SerializableTests.serializable(HashMap.catsDataHashForHashMap[Int, String]))

  checkAll("HashMap[Int, Int] with Option",
           UnorderedTraverseTests[HashMap[Int, *]].unorderedTraverse[Int, Int, Int, Option, Option]
  )
  checkAll("UnorderedTraverse[HashMap[Int, *]]",
           SerializableTests.serializable(HashMap.catsDataUnorderedTraverseForHashMap[Int])
  )

  checkAll("HashMap[Int, String]", CommutativeMonoidTests[HashMap[Int, Int]].commutativeMonoid)
  checkAll("CommutativeMonoid[HashMap[Int, Int]]",
           SerializableTests.serializable(HashMap.catsDataCommutativeMonoidForHashMap[Int, Int])
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

  def genStringKey(hm: HashMap[String, Int]): Gen[String] =
    if (hm.isEmpty)
      arbitrary[String]
    else
      Gen.oneOf(
        Gen.oneOf(hm.keysIterator.toList).map(collidingString),
        arbitrary[String]
      )

  // Key-value pairs with the last binding for each distinct key
  def distinctBindings[K, V](kvs: List[(K, V)]) =
    kvs.reverse.distinctBy(_._1).reverse

  test("show") {
    assert(HashMap("a" -> 1, "b" -> 2, "c" -> 3).show === "HashMap(c -> 3, a -> 1, b -> 2)")
    assert(HashMap.empty[String, Int].show === "HashMap()")
  }

  test("isEmpty and nonEmpty") {
    assert(HashMap.empty[Int, Int].isEmpty)
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromSeq(kvs)
      assert(hashMap.isEmpty === kvs.isEmpty)
      assert(hashMap.nonEmpty === kvs.nonEmpty)
    }
  }

  test("===") {
    forAll { (hashMap: HashMap[Int, String]) =>
      assert(hashMap === hashMap)
    }

    forAll { (left: HashMap[Int, String], right: HashMap[Int, String]) =>
      val leftKvs = distinctBindings(left.iterator.toList)
      val rightKvs = distinctBindings(right.iterator.toList)
      assert((distinctBindings(leftKvs) === distinctBindings(rightKvs)) === (left === right))
    }
  }

  test("size") {
    assert(HashMap.empty[Int, String].size === 0)
    assert(HashMap(1 -> "a", 2 -> "b", 3 -> "c").size === 3)
    assert(HashMap("Aa" -> 1, "BB" -> 2).size == 2)

    forAll { (hashMap: HashMap[Int, String]) =>
      val distinctKvs = distinctBindings(hashMap.iterator.toList)
      assert(hashMap.size === distinctKvs.size)
    }
  }

  test("get") {
    forAll { (hashMap: HashMap[Int, String]) =>
      distinctBindings(hashMap.iterator.toList)
        .foreach { case (k, v) =>
          hashMap.get(k) === Some(v)
        }
    }
  }

  test("updated") {
    forAll { (initial: HashMap[String, Int], value: Int) =>
      forAll(genStringKey(initial)) { (key: String) =>
        val updated = initial.updated(key, value)
        assert(updated.contains(key))
        assert(updated.get(key) === Some(value))
        if (initial.contains(key))
          assert(updated.size === initial.size)
        else
          assert(updated.size === (initial.size + 1))
      }
    }
  }

  test("removed") {
    forAll { (initial: HashMap[String, Int]) =>
      forAll(genStringKey(initial)) { (key: String) =>
        val removed = initial.removed(key)
        assert(!removed.contains(key))
        assert(removed.get(key) === None)
        if (initial.contains(key))
          assert(removed.size === (initial.size - 1))
        else
          assert(removed === initial)
      }
    }
  }

  test("concat") {
    forAll { (left: HashMap[Int, String], right: HashMap[Int, String]) =>
      val leftKvs = left.iterator.toList
      val rightKvs = right.iterator.toList
      val distinctKvs = distinctBindings(leftKvs ++ rightKvs)
      val both = left.concat(right)
      assert(both === HashMap.fromSeq(leftKvs ++ rightKvs))
      assert(both.size === distinctKvs.size)
      distinctKvs.foreach { case (k, v) =>
        assert(both.contains(k))
        assert(both.get(k) === Some(v))
        if (right.contains(k))
          assert(both.get(k) === right.get(k))
        else
          assert(both.get(k) === left.get(k))
      }
    }
  }

  property("fromSeq consistent with contains") {
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromSeq(kvs)

      kvs.foreach { case (k, _) =>
        assert(hashMap.contains(k))
      }
    }
  }

  property("fromIterableOnce consistent with contains") {
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromIterableOnce(kvs.view)

      kvs.foreach { case (k, _) =>
        assert(hashMap.contains(k))
      }
    }
  }

  property("fromFoldable consistent with contains") {
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromFoldable(kvs)

      kvs.foreach { case (k, _) =>
        assert(hashMap.contains(k))
      }
    }
  }

  property("iterator consistent with contains") {
    forAll { (hashMap: HashMap[Int, String]) =>
      val iterated = mutable.ListBuffer[(Int, String)]()
      hashMap.iterator.foreach { iterated += _ }
      assert(iterated.toList.distinctBy(_._1) === iterated.toList)
      assert(iterated.forall { case (k, _) => hashMap.contains(k) })
    }
  }

  property("foreach consistent with contains") {
    forAll { (hashMap: HashMap[Int, String]) =>
      val foreached = mutable.ListBuffer[(Int, String)]()
      hashMap.foreach { (i: Int, s: String) => foreached += ((i, s)) }
      assert(foreached.toList.distinct === foreached.toList)
      assert(foreached.forall { case (k, _) => hashMap.contains(k) })
    }
  }

  property("foreach and iterator consistent") {
    forAll { (hashMap: HashMap[Int, String]) =>
      val iterated = mutable.ListBuffer[(Int, String)]()
      val foreached = mutable.ListBuffer[(Int, String)]()
      hashMap.iterator.foreach { iterated += _ }
      hashMap.foreach { (i: Int, s: String) => foreached += ((i, s)) }
      assert(foreached.forall(iterated.contains))
      assert(iterated.forall(foreached.contains))
    }
  }

  property("size consistent with iterator") {
    forAll { (hashMap: HashMap[Int, String]) =>
      var size = 0
      hashMap.iterator.foreach { _ => size += 1 }
      assert(hashMap.size === size)
    }
  }

  property("size consistent with foreach") {
    forAll { (hashMap: HashMap[Int, String]) =>
      var size = 0
      hashMap.foreach { case _ => size += 1 }
      assert(hashMap.size === size)
    }
  }

  property("show consistent with ===") {
    forAll { (left: HashMap[Int, String], right: HashMap[Int, String]) =>
      if (left.show === right.show)
        assert(left === right)
    }
  }

  property("toString consistent with equals") {
    forAll { (left: HashMap[Int, String], right: HashMap[Int, String]) =>
      if (left.toString == right.toString)
        assertEquals(left, right)
    }
  }
}
