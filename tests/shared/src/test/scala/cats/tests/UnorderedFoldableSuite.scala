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

import cats.UnorderedFoldable
import cats.kernel.CommutativeMonoid
import cats.laws.discipline.UnorderedFoldableTests
import cats.syntax.unorderedFoldable.*
import org.scalacheck.Arbitrary
import cats.syntax.eq.*
import org.scalacheck.Prop.*

sealed abstract class UnorderedFoldableSuite[F[_]](name: String)(implicit
  ArbFString: Arbitrary[F[String]],
  ArbFInt: Arbitrary[F[Int]]
) extends CatsSuite {

  def iterator[T](fa: F[T]): Iterator[T]
  def specializedUnorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B

  private[this] val instance: UnorderedFoldable[F] =
    new UnorderedFoldable[F] {
      def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B =
        specializedUnorderedFoldMap(fa)(f)
    }

  test(s"UnorderedFoldable[$name].isEmpty") {
    forAll { (fa: F[String]) =>
      assert(instance.isEmpty(fa) === (instance.size(fa) === 0L))
    }
  }

  test(s"UnorderedFoldable[$name].nonEmpty") {
    forAll { (fa: F[String]) =>
      assert(instance.nonEmpty(fa) === (instance.size(fa) > 0L))
    }
  }

  test(s"UnorderedFoldable[$name].count") {
    forAll { (fa: F[String], p: String => Boolean) =>
      implicit val F: UnorderedFoldable[F] = instance
      assert(fa.count(p) === (iterator(fa).count(p).toLong))
    }
  }

  test(s"UnorderedFoldable[$name].size") {
    forAll { (fa: F[String]) =>
      implicit val F: UnorderedFoldable[F] = instance
      assert(fa.count(Function.const(true)) === (fa.size))
    }
  }

  test(s"UnorderedFoldable[$name].contains") {
    forAll { (fa: F[String], v: String) =>
      implicit val F: UnorderedFoldable[F] = instance
      assert(fa.contains_(v) === (iterator(fa).contains(v)))
    }
  }

  checkAll("F[Int]", UnorderedFoldableTests[F](using instance).unorderedFoldable[Int, Int])
}

final class UnorderedFoldableSetSuite extends UnorderedFoldableSuite[Set]("set") {
  def iterator[T](set: Set[T]): Iterator[T] = set.iterator
  def specializedUnorderedFoldMap[A, B: CommutativeMonoid](fa: Set[A])(f: A => B): B =
    UnorderedFoldable[Set].unorderedFoldMap(fa)(f)
}

final class UnorderedFoldableMapSuite extends UnorderedFoldableSuite[Map[String, *]]("map") {
  def iterator[T](map: Map[String, T]): Iterator[T] = map.valuesIterator
  def specializedUnorderedFoldMap[A, B: CommutativeMonoid](fa: Map[String, A])(f: A => B): B =
    UnorderedFoldable[Map[String, *]].unorderedFoldMap(fa)(f)
}

sealed abstract class SpecializedUnorderedFoldableSuite[F[_]: UnorderedFoldable](name: String)(implicit
  ArbFString: Arbitrary[F[String]]
) extends CatsSuite {

  def iterator[T](fa: F[T]): Iterator[T]

  test(s"Specialized UnorderedFoldable[$name].count") {
    forAll { (fa: F[String], p: String => Boolean) =>
      assert(fa.count(p) === (iterator(fa).count(p).toLong))
    }
  }
}

final class SpecializedUnorderedFoldableSetSuite extends SpecializedUnorderedFoldableSuite[Set]("set") {
  def iterator[T](set: Set[T]): Iterator[T] = set.iterator
}

final class SpecializedUnorderedFoldableMapSuite extends SpecializedUnorderedFoldableSuite[Map[String, *]]("map") {
  def iterator[T](map: Map[String, T]): Iterator[T] = map.valuesIterator
}
