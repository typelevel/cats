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

package cats
package tests

import cats.data.Chain
import cats.data.Chain.`:==`
import cats.data.Chain.==:
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.HashTests
import cats.kernel.laws.discipline.MonoidTests
import cats.kernel.laws.discipline.OrderTests
import cats.kernel.laws.discipline.PartialOrderTests
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.eq._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop._

class ChainSuite extends CatsSuite {
  checkAll("Chain[Int]", AlternativeTests[Chain].alternative[Int, Int, Int])
  checkAll("Alternative[Chain]", SerializableTests.serializable(Alternative[Chain]))

  // Traverse behaviour discriminates on the Runtime type of the Applicative
  checkAll("Chain[Int] with Option", TraverseTests[Chain].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Chain[Int] with Eval", TraverseTests[Chain].traverse[Int, Int, Int, Set[Int], Eval, Eval])
  checkAll("Traverse[Chain]", SerializableTests.serializable(Traverse[Chain]))

  checkAll("Chain[Int]", MonadTests[Chain].monad[Int, Int, Int])
  checkAll("Monad[Chain]", SerializableTests.serializable(Monad[Chain]))

  checkAll("Chain[Int]", CoflatMapTests[Chain].coflatMap[Int, Int, Int])
  checkAll("Coflatmap[Chain]", SerializableTests.serializable(CoflatMap[Chain]))

  checkAll("Chain[Int]", MonoidTests[Chain[Int]].monoid)
  checkAll("Monoid[Chain]", SerializableTests.serializable(Monoid[Chain[Int]]))

  checkAll("Chain[Int]", OrderTests[Chain[Int]].order)
  checkAll("Order[Chain]", SerializableTests.serializable(Order[Chain[Int]]))

  checkAll("Chain[Int]", AlignTests[Chain].align[Int, Int, Int, Int])
  checkAll("Align[Chain]", SerializableTests.serializable(Align[Chain]))

  checkAll("Chain[Int]", TraverseFilterTests[Chain].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Chain]", SerializableTests.serializable(TraverseFilter[Chain]))

  checkAll("Chain[Int]", ShortCircuitingTests[Chain].foldable[Int])
  checkAll("Chain[Int]", ShortCircuitingTests[Chain].traverseFilter[Int])

  {
    implicit val partialOrder: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]
    checkAll("Chain[ListWrapper[Int]]", PartialOrderTests[Chain[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[Chain[ListWrapper[Int]]",
             SerializableTests.serializable(PartialOrder[Chain[ListWrapper[Int]]])
    )
  }

  {
    implicit val eqv: Eq[ListWrapper[Int]] = ListWrapper.eqv[Int]
    checkAll("Chain[ListWrapper[Int]]", EqTests[Chain[ListWrapper[Int]]].eqv)
    checkAll("Eq[Chain[ListWrapper[Int]]", SerializableTests.serializable(Eq[Chain[ListWrapper[Int]]]))
  }

  {
    implicit val hash: Hash[ListWrapper[Int]] = ListWrapper.hash[Int]
    checkAll("Chain[ListWrapper[Int]]", HashTests[Chain[ListWrapper[Int]]].hash)
    checkAll("Hash[Chain[ListWrapper[Int]]", SerializableTests.serializable(Hash[Chain[ListWrapper[Int]]]))
  }

  test("show") {
    assert(Show[Chain[Int]].show(Chain(1, 2, 3)) === "Chain(1, 2, 3)")
    assert(Chain.empty[Int].show === "Chain()")
    forAll { (l: Chain[String]) =>
      assert(l.show === (l.toString))
    }
  }

  test("headOption") {
    forAll { (s: Seq[Int]) =>
      assert(Chain.fromSeq(s).headOption === (s.headOption))
    }
  }

  test("lastOption") {
    forAll { (c: Chain[Int]) =>
      assert(c.lastOption === (c.toList.lastOption))
    }
  }

  test("fromOption") {
    forAll { (o: Option[Int]) =>
      assert(Chain.fromOption(o).toList == o.toList)
    }
  }

  test("seq-like pattern match") {
    Chain(1, 2, 3) match {
      case Chain(a, b, c) => assert((a, b, c) === ((1, 2, 3)))
      case other          => fail(other.show)
    }

    Chain(1, 2, 3) match {
      case h ==: t => assert((h, t) === 1 -> Chain(2, 3))
      case other   => fail(other.show)
    }

    Chain(1, 2, 3) match {
      case init :== last => assert((init, last) === Chain(1, 2) -> 3)
      case other         => fail(other.show)
    }
  }

  test("size is consistent with toList.size") {
    forAll { (ci: Chain[Int]) =>
      assert(ci.size.toInt === (ci.toList.size))
    }
  }

  test("knownSize should be consistent with size") {
    forAll { (cu: Chain[Unit]) =>
      val expected = cu.size match {
        case size @ (0L | 1L) => size
        case _                => -1L
      }

      assertEquals(cu.knownSize, expected)
    }
  }

  test("lengthCompare and sizeCompare should be consistent with length and size") {
    forAll { (cu: Chain[Unit], diff: Byte) =>
      val testLen = cu.length + diff
      val testSize = cu.size + diff

      val expectedSignumLen = math.signum(cu.length.compareTo(testLen))
      val expectedSignumSize = math.signum(cu.size.compareTo(testSize))

      val obtainedSignumLen = math.signum(cu.lengthCompare(testLen))
      val obtainedSignumSize = math.signum(cu.sizeCompare(testSize))

      assertEquals(obtainedSignumLen, expectedSignumLen)
      assertEquals(obtainedSignumSize, expectedSignumSize)
    }
  }
  test("lengthCompare and sizeCompare should be consistent with length and size (Chain.Wrap stressed)") {
    //
    // Similar to the previous test but stresses handling Chain.Wrap cases.
    //

    // Range as `Seq` can has huge size without keeping any elements in it.
    val seqGen: Gen[Seq[Int]] = Gen.chooseNum(2, Int.MaxValue).map(0 until _)
    val testValGen: Gen[Long] = Arbitrary.arbitrary[Long]

    // Disable shrinking since it can lead to re-building of range into a regular `Seq`.
    forAllNoShrink(seqGen, testValGen) { (seq, testVal) =>
      val ci = Chain.fromSeq(seq) // should produce `Chain.Wrap`

      val expectedSignumLen = math.signum(seq.length.toLong.compareTo(testVal))
      val expectedSignumSize = math.signum(seq.size.toLong.compareTo(testVal))

      val obtainedSignumLen = math.signum(ci.lengthCompare(testVal))
      val obtainedSignumSize = math.signum(ci.sizeCompare(testVal))

      assertEquals(obtainedSignumLen, expectedSignumLen)
      assertEquals(obtainedSignumSize, expectedSignumSize)
    }
  }

  test("filterNot and then exists should always be false") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      assert(ci.filterNot(f).exists(f) === false)
    }
  }

  test("filter and then forall should always be true") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      assert(ci.filter(f).forall(f) === true)
    }
  }

  test("exists should be consistent with find + isDefined") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      assert(ci.exists(f) === (ci.find(f).isDefined))
    }
  }

  test("deleteFirst consistent with find") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      assert(ci.find(f) === (ci.deleteFirst(f).map(_._1)))
    }
  }

  test("filterNot element and then contains should be false") {
    forAll { (ci: Chain[Int], i: Int) =>
      assert(ci.filterNot(_ === i).contains(i) === false)
    }
  }

  test("Always nonempty after cons") {
    forAll { (ci: Chain[Int], i: Int) =>
      assert((i +: ci).nonEmpty === true)
    }
  }

  test("fromOption should be consistent with one") {
    val expected = Chain.one(())
    val obtained = Chain.fromOption(Some(()))

    assert(obtained.getClass eq expected.getClass)
    assert(obtained === expected)
  }

  test("fromSeq should be consistent with one") {
    val expected = Chain.one(())
    val obtained = Chain.fromSeq(() :: Nil)
    assert(obtained.getClass eq expected.getClass)
    assert(obtained === expected)
  }

  test("fromIterableOnce should be consistent with one") {
    val expected = Chain.one(())
    val obtained = Chain.fromIterableOnce(Iterator.single(()))
    assert(obtained.getClass eq expected.getClass)
    assert(obtained === expected)
  }

  test("fromSeq . toVector is id") {
    forAll { (ci: Chain[Int]) =>
      assert(Chain.fromSeq(ci.toVector) === ci)
    }
  }

  test("fromSeq . toList . iterator is id") {
    forAll { (ci: Chain[Int]) =>
      assert(Chain.fromSeq(ci.iterator.toList) === ci)
    }
  }

  test("zipWith consistent with List#zip and then List#map") {
    forAll { (a: Chain[String], b: Chain[Int], f: (String, Int) => Int) =>
      assert(a.zipWith(b)(f).toList === a.toList.zip(b.toList).map { case (x, y) => f(x, y) })
    }
  }

  test("groupBy consistent with List#groupBy") {
    forAll { (cs: Chain[String], key: String => Int) =>
      val result = cs.groupBy(key).map { case (k, v) => (k, v.toList) }.toMap
      val expected = cs.toList.groupBy(key).toMap
      assert(result === expected)
    }
  }

  test("groupMap consistent with List#groupBy + Map#mapValues") {
    forAll { (cs: Chain[String], key: String => String, f: String => Int) =>
      val result = cs.groupMap(key)(f).map { case (k, v) => (k, v.toList) }.toMap
      val expected = cs.toList.groupBy(key).map { case (k, v) => (k, v.map(f)) }
      assert(result === expected)
    }
  }

  test("groupMapReduce consistent with List#groupBy + Map#mapValues + List#reduce") {
    forAll { (cs: Chain[String], key: String => String, f: String => Int) =>
      val result = cs.groupMapReduce(key)(f).toMap
      val expected = cs.toList.groupBy(key).map { case (k, v) => (k, v.map(f).reduce(Semigroup[Int].combine)) }
      assert(result === expected)
    }
  }

  test("groupMapReduceWith consistent with List#groupBy + Map#mapValues + List#reduce") {
    forAll { (cs: Chain[String], key: String => String, f: String => Int, combine: (Int, Int) => Int) =>
      val result = cs.groupMapReduceWith(key)(f)(combine).toMap
      val expected = cs.toList.groupBy(key).map { case (k, v) => (k, v.map(f).reduce(combine)) }
      assert(result === expected)
    }
  }

  test("zipWithIndex is consistent with toList.zipWithIndex") {
    forAll { (ci: Chain[Int]) =>
      assert(ci.zipWithIndex.toList === (ci.toList.zipWithIndex))
    }
  }

  test("zipWithIndex is stack-safe for a large chain constructed using concatenations") {
    val list = List.fill(10000)(1)
    val chain = list.foldLeft(Chain.empty[Int]) { case (acc, next) => acc.concat(Chain(next)) }
    chain.zipWithIndex.toList === (list.zipWithIndex)
  }

  test("sortBy is consistent with toList.sortBy") {
    forAll { (ci: Chain[Int], f: Int => String) =>
      assert(ci.sortBy(f).toList === (ci.toList.sortBy(f)))
    }
  }

  test("sorted is consistent with toList.sorted") {
    forAll { (ci: Chain[Int]) =>
      assert(ci.sorted.toList === (ci.toList.sorted))
    }
  }

  test("reverse . reverse is id") {
    forAll { (ci: Chain[Int]) =>
      assert(ci.reverse.reverse === ci)
    }
  }

  test("reverse consistent with List#reverse") {
    forAll { (ci: Chain[Int]) =>
      assert(ci.reverse.toList === (ci.toList.reverse))
    }
  }

  test("(a ++ b).isEmpty ==> a.isEmpty and b.isEmpty") {
    forAll { (a: Chain[Int], b: Chain[Int]) =>
      assert((a ++ b).nonEmpty || (a.isEmpty && b.isEmpty))
    }
  }

  test("a.isEmpty == (a eq Chain.nil)") {
    assert(Chain.fromSeq(Nil) eq Chain.nil)
    assert(Chain.fromOption(None) eq Chain.nil)
    assert(Chain.fromIterableOnce(Iterator.empty) eq Chain.nil)

    forAll { (a: Chain[Int]) =>
      assert(a.isEmpty == (a eq Chain.nil))
    }
  }

  test("(nil ++ a) eq a") {
    forAll { (a: Chain[Int]) =>
      assert((Chain.nil ++ a) eq a)
      assert((a ++ Chain.nil) eq a)
    }
  }

  test("Chain.iterator.next should throw NoSuchElementException") {
    forAll { (a: Chain[Int]) =>
      val it = a.iterator

      while (it.hasNext) it.next()

      intercept[java.util.NoSuchElementException] {
        it.next()
      }

      val rit = a.reverseIterator

      while (rit.hasNext) rit.next()

      intercept[java.util.NoSuchElementException] {
        rit.next()
      }
      ()
    }
  }

  test("Chain#distinct is consistent with List#distinct") {
    forAll { (a: Chain[Int]) =>
      assert(a.distinct.toList === (a.toList.distinct))
    }
  }

  test("Chain#distinctBy is consistent with List#distinctBy") {
    forAll { (a: Chain[Int], f: Int => String) =>
      assertEquals(a.distinctBy(f).toList, a.toList.distinctBy(f))
    }
  }

  test("=== is consistent with == (issue #2540)") {
    assertEquals(Chain.one(1) |+| Chain.one(2) |+| Chain.one(3), Chain.fromSeq(List(1, 2, 3)))

    forAll { (a: Chain[Int], b: Chain[Int]) =>
      assert((a === b) === (a == b))
    }
  }

  test("== returns false for non-Chains") {
    forAll { (a: Chain[Int], b: Int) =>
      assert((a.equals(b)) === false)
    }
  }

  test("== returns false for Chains of different element types") {
    forAll { (a: Chain[Option[String]], b: Chain[String]) =>
      assert((a.equals(b)) === (a.isEmpty && b.isEmpty))
    }
  }

  test("Chain#hashCode is consistent with List#hashCode") {
    forAll { (x: Chain[Int]) =>
      assert(x.hashCode === (x.toList.hashCode))
    }
  }

  test("Chain#takeWhile is consistent with List#takeWhile") {
    forAll { (x: Chain[Int], p: Int => Boolean) =>
      assert(x.takeWhile(p).toList === (x.toList.takeWhile(p)))
    }
  }

  test("Chain#dropWhile is consistent with List#dropWhile") {
    forAll { (x: Chain[Int], p: Int => Boolean) =>
      assert(x.dropWhile(p).toList === (x.toList.dropWhile(p)))
    }
  }

  test("Chain#get is consistent with List#lift") {
    forAll { (x: Chain[Int], idx: Int) =>
      assert(x.get(idx.toLong) === (x.toList.lift(idx)))
    }
  }

  test("traverse is stack-safe") {
    val chain = (0 until 100000).map(Chain.one).reduce(_.concat(_))
    val sumAll = Traverse[Chain]
      .traverse(chain) { i => () => i }
      .apply()
      .iterator
      .sum

    assert(sumAll == chain.iterator.sum)
  }

  test("foldRight(b)(fn) == toList.foldRight(b)(fn)") {
    forAll { (chain: Chain[Int], init: Long, fn: (Int, Long) => Long) =>
      assert(chain.foldRight(init)(fn) == chain.toList.foldRight(init)(fn))
    }
  }
}
