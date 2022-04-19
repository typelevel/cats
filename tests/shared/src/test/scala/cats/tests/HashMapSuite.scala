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
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.UnorderedTraverseTests
import cats.laws.discipline.SerializableTests
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scala.collection.mutable

class HashMapSuite extends CatsSuite {
  // Examples from https://stackoverflow.com/questions/9406775/why-does-string-hashcode-in-java-have-many-conflicts
  val collidingStrings = for {
    left <- ('A' to 'Y').toList
    first = List(left, 'a').mkString
    second = List((left + 1).toChar, 'B').mkString
  } yield (first, second)

  val collidingKv = Gen.oneOf(collidingStrings).flatMap { case (leftStr, rightStr) =>
    for {
      leftInt <- arbitrary[Int]
      rightInt <- arbitrary[Int]
    } yield (leftStr -> leftInt, rightStr -> rightInt)
  }

  val collidingKvs = Gen.listOf(collidingKv)

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

  // Key-value pairs with the last binding for each distinct key
  def distinctBindings[K, V](kvs: List[(K, V)]) =
    kvs.reverse.distinctBy(_._1).reverse

  test("show") {
    assert(HashMap("a" -> 1, "b" -> 2, "c" -> 3).show === "HashMap(a -> 1, b -> 2, c -> 3)")
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
    forAll { (kvs: List[(Int, String)]) =>
      val left = HashMap.fromSeq(kvs)
      val right = HashMap.fromSeq(kvs)
      assert(left === right)
    }

    forAll { (leftKvs: List[(Int, String)], rightKvs: List[(Int, String)]) =>
      val left = HashMap.fromSeq(leftKvs)
      val right = HashMap.fromSeq(rightKvs)
      assert((distinctBindings(leftKvs) === distinctBindings(rightKvs)) === (left === right))
    }
  }

  test("size") {
    assert(HashMap.empty[Int, String].size === 0)
    assert(HashMap(1 -> "a", 2 -> "b", 3 -> "c").size === 3)
    assert(HashMap("Aa" -> 1, "BB" -> 2).size == 2)

    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromSeq(kvs)
      assert(hashMap.size === distinctBindings(kvs).size)
    }
  }

  test("get") {
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromSeq(kvs)
      distinctBindings(kvs).foreach { case (k, v) =>
        hashMap.get(k) === Some(v)
      }
    }
  }

  test("concat") {
    forAll { (leftKvs: List[(Int, String)], rightKvs: List[(Int, String)]) =>
      val distinctKvs = distinctBindings(leftKvs ++ rightKvs)
      val left = HashMap.fromSeq(leftKvs)
      val right = HashMap.fromSeq(rightKvs)
      val both = left.concat(right)
      assert(both === HashMap.fromSeq(leftKvs ++ rightKvs))
      distinctKvs.foreach { case (k, v) =>
        assert(both.contains(k))
        assert(both.get(k) === Some(v))
      }
    }
  }

  property("Empty HashMap never contains") {
    forAll { (i: Int) =>
      assert(!HashMap.empty[Int, String].contains(i))
    }
  }

  property("Empty HashMap add consistent with contains") {
    forAll { (i: Int, s: String) =>
      assert(HashMap.empty[Int, String].add(i, s).contains(i))
    }
  }

  property("Empty HashMap add and remove consistent with contains") {
    forAll { (i: Int, s: String) =>
      assert(!HashMap.empty[Int, String].add(i, s).remove(i).contains(i))
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

  property("remove consistent with contains") {
    forAll { (kvs: List[(Int, String)]) =>
      val distinctKvs = distinctBindings(kvs)
      val hashMap = HashMap.fromSeq(kvs)

      distinctKvs.foldLeft(hashMap) { case (hm, (i, _)) =>
        if (!hm.contains(i)) {
          println(hm)
          println(i)
          println(hm.contains(i))
        }
        assert(hm.contains(i))
        assert(!hm.remove(i).contains(i))
        hm.remove(i)
      }

      ()
    }
  }

  property("add and remove with collisions consistent with contains") {
    forAll(collidingKvs) { (collisions: List[((String, Int), (String, Int))]) =>

      val distinctCollisions = collisions.distinctBy(_._1._1)

      val hashMap = distinctCollisions.foldLeft(HashMap.empty[String, Int]) { case (hm, ((lk, lv), (rk, rv))) =>
        hm.add(lk, lv).add(rk, rv)
      }

      distinctCollisions.foldLeft(hashMap) { case (hm, ((lk, _), (rk, _))) =>
        assert(hm.contains(lk))
        assert(hm.contains(rk))

        val removeL = hm.remove(lk)
        assert(removeL.contains(rk))
        assert(!removeL.contains(lk))

        val removeR = hm.remove(rk)
        assert(removeR.contains(lk))
        assert(!removeR.contains(rk))

        val removeLR = removeL.remove(rk)
        assert(!removeLR.contains(lk))
        assert(!removeLR.contains(rk))

        removeLR
      }

      ()
    }
  }

  property("iterator consistent with contains") {
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromSeq(kvs)
      val distinctKvs = distinctBindings(kvs)

      val iterated = mutable.ListBuffer[(Int, String)]()
      hashMap.iterator.foreach { iterated += _ }

      assert(distinctKvs.forall(iterated.contains))
      assert(iterated.forall(distinctKvs.contains))
      assert(iterated.toList.distinctBy(_._1) === iterated.toList)
      assert(iterated.forall { case (k, _) => hashMap.contains(k) })
    }
  }

  property("iterator consistent with reverseIterator") {
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromSeq(kvs)

      val iterated = mutable.ListBuffer[(Int, String)]()
      val reverseIterated = mutable.ListBuffer[(Int, String)]()
      hashMap.iterator.foreach { iterated += _ }
      hashMap.reverseIterator.foreach { reverseIterated += _ }

      assert(iterated.toList === reverseIterated.toList.reverse)
    }
  }

  property("foreach consistent with contains") {
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromSeq(kvs)
      val distinctKvs = distinctBindings(kvs)

      val foreached = mutable.ListBuffer[(Int, String)]()
      hashMap.foreach { (i: Int, s: String) => foreached += ((i, s)) }

      assert(distinctKvs.forall(foreached.contains))
      assert(foreached.forall(distinctKvs.contains))
      assert(foreached.toList.distinct === foreached.toList)
      assert(foreached.forall { case (k, _) => hashMap.contains(k) })
    }
  }

  property("foreach and iterator consistent") {
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromSeq(kvs)

      val iterated = mutable.ListBuffer[(Int, String)]()
      val foreached = mutable.ListBuffer[(Int, String)]()
      hashMap.iterator.foreach { iterated += _ }
      hashMap.foreach { (i: Int, s: String) => foreached += ((i, s)) }

      assert(foreached.forall(iterated.contains))
      assert(iterated.forall(foreached.contains))
    }
  }

  property("size consistent with iterator") {
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromSeq(kvs)

      var size = 0
      hashMap.iterator.foreach { _ => size += 1 }

      assert(hashMap.size === size)
    }
  }

  property("size consistent with foreach") {
    forAll { (kvs: List[(Int, String)]) =>
      val hashMap = HashMap.fromSeq(kvs)

      var size = 0
      hashMap.foreach { case _ => size += 1 }

      assert(hashMap.size === size)
    }
  }
}
