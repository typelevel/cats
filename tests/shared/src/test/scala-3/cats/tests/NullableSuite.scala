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

import cats.{Applicative, Eq, Foldable, Functor, Hash, Monad, Monoid, Order, PartialOrder, Semigroup, Show}
import cats.data.Nullable
import cats.data.Nullable.*
import cats.kernel.laws.discipline.{EqTests, HashTests, MonoidTests, OrderTests, PartialOrderTests, SemigroupTests}
import cats.laws.discipline.{FoldableTests, SerializableTests}
import cats.syntax.eq.*
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Prop.*

class NullableSuite extends CatsSuite {

  private type NestedNullable[A] = Nullable[Nullable[A]]

  private given Foldable[NestedNullable] = Foldable[Nullable].compose[Nullable]

  given [A](using A: Arbitrary[A]): Arbitrary[Nullable[A]] =
    Arbitrary(Gen.oneOf(Gen.const(Nullable.empty[A]), A.arbitrary.map(Nullable(_))))

  given [A](using A: Cogen[A]): Cogen[Nullable[A]] =
    Cogen[Option[A]].contramap(_.toOption)

  checkAll("Nullable[Int]", EqTests[Nullable[Int]].eqv)
  checkAll("Nullable[Nullable[Int]]", EqTests[NestedNullable[Int]].eqv)
  checkAll("Eq[Nullable[Int]]", SerializableTests.serializable(Eq[Nullable[Int]]))
  checkAll("Eq[Nullable[Nullable[Int]]]", SerializableTests.serializable(Eq[NestedNullable[Int]]))

  checkAll("Nullable[Int]", HashTests[Nullable[Int]].hash)
  checkAll("Nullable[Nullable[Int]]", HashTests[NestedNullable[Int]].hash)
  checkAll("Hash[Nullable[Int]]", SerializableTests.serializable(Hash[Nullable[Int]]))
  checkAll("Hash[Nullable[Nullable[Int]]]", SerializableTests.serializable(Hash[NestedNullable[Int]]))

  checkAll("Nullable[Int]", PartialOrderTests[Nullable[Int]].partialOrder)
  checkAll("Nullable[Nullable[Int]]", PartialOrderTests[NestedNullable[Int]].partialOrder)
  checkAll("PartialOrder[Nullable[Int]]", SerializableTests.serializable(PartialOrder[Nullable[Int]]))
  checkAll(
    "PartialOrder[Nullable[Nullable[Int]]]",
    SerializableTests.serializable(PartialOrder[NestedNullable[Int]])
  )

  checkAll("Nullable[Int]", OrderTests[Nullable[Int]].order)
  checkAll("Nullable[Nullable[Int]]", OrderTests[NestedNullable[Int]].order)
  checkAll("Order[Nullable[Int]]", SerializableTests.serializable(Order[Nullable[Int]]))
  checkAll("Order[Nullable[Nullable[Int]]]", SerializableTests.serializable(Order[NestedNullable[Int]]))

  checkAll("Nullable[Int]", SemigroupTests[Nullable[Int]].semigroup)
  checkAll("Semigroup[Nullable[Int]]", SerializableTests.serializable(Semigroup[Nullable[Int]]))

  checkAll("Nullable[Int]", MonoidTests[Nullable[Int]].monoid)
  checkAll("Nullable[Nullable[Int]]", MonoidTests[NestedNullable[Int]].monoid)
  checkAll("Monoid[Nullable[Int]]", SerializableTests.serializable(Monoid[Nullable[Int]]))
  checkAll("Monoid[Nullable[Nullable[Int]]]", SerializableTests.serializable(Monoid[NestedNullable[Int]]))

  checkAll("Show[Nullable[Int]]", SerializableTests.serializable(Show[Nullable[Int]]))
  checkAll("Show[Nullable[Nullable[Int]]]", SerializableTests.serializable(Show[NestedNullable[Int]]))

  checkAll("Foldable[Nullable]", FoldableTests[Nullable].foldable[Int, Int])
  checkAll("Foldable[Nullable].compose[Nullable]", FoldableTests[NestedNullable].foldable[Int, Int])
  checkAll("Foldable[Nullable]", SerializableTests.serializable(Foldable[Nullable]))

  test("fold") {
    val nonEmpty: Nullable[Int] = Nullable(1)
    val empty: Nullable[Int] = Nullable.empty

    assert(nonEmpty.fold(0)(_ + 1) === 2)
    assert(empty.fold(0)(_ + 1) === 0)
  }

  test("fold homomorphism with Option") {
    forAll { (n: Nullable[Int], x: Int, fn: Int => Int) =>
      assert(n.fold(x)(fn) === n.toOption.fold(x)(fn))
    }
  }

  test("fold infers union result type") {
    val nonEmpty: Nullable[String] = Nullable("long")
    val empty: Nullable[String] = Nullable.empty

    val nonEmptyIntOrLong: Int | Long = nonEmpty.fold(0)(_ => 1L)
    val emptyIntOrLong: Int | Long = empty.fold(0)(_ => 1L)

    assert(nonEmptyIntOrLong == 1L)
    assert(emptyIntOrLong == 0)
  }

  test("fromOption / toOption round trip") {
    forAll { (opt: Option[Int]) =>
      assert(Nullable.fromOption(opt).toOption === opt)
    }
  }

  test("iterator is consistent with Option iterator") {
    forAll { (nullable: Nullable[Int]) =>
      assert(nullable.iterator.toList === nullable.toOption.iterator.toList)
    }
  }

  test("flatten is consistent with Nullable identity and Option view") {
    forAll { (nullable: Nullable[Int]) =>
      val nested: Nullable[Nullable[Int]] = Nullable(nullable)
      assert(nested.flatten === nullable)
      assert(nested.flatten.toOption === nullable.toOption)
    }
  }

  test("flatten collapses nested empty and nested non-empty values") {
    val nestedEmpty: Nullable[Nullable[Int]] = Nullable(Nullable.empty[Int])
    assert(nestedEmpty.flatten === Nullable.empty[Int])

    forAll { (x: Int) =>
      val nestedNonEmpty: Nullable[Nullable[Int]] = Nullable(Nullable(x))
      assert(nestedNonEmpty.flatten === Nullable(x))
    }
  }

  test("flatten removes any number of nested Nullable wrappers down to one layer") {
    val tripleEmpty: Nullable[Nullable[Nullable[Int]]] = Nullable(Nullable(Nullable.empty[Int]))
    val tripleNonEmpty: Nullable[Nullable[Nullable[Int]]] = Nullable(Nullable(Nullable(1)))

    val flattenedEmpty: Nullable[Int] = tripleEmpty.flatten
    val flattenedNonEmpty: Nullable[Int] = tripleNonEmpty.flatten

    assert(flattenedEmpty === Nullable.empty[Int])
    assert(flattenedNonEmpty === Nullable(1))
  }

  test("minimal Functor[Nullable] fails composition when composed with itself") {
    val candidate = new Functor[Nullable] {
      def map[A, B](fa: Nullable[A])(f: A => B): Nullable[B] =
        fa.transform(f)
    }

    val composed: Functor[NestedNullable] = candidate.compose[Nullable](using candidate)

    val nested: NestedNullable[Int] = Nullable(Nullable(1))
    val f: Int => String = _ => null
    val g: String => Int = _ => 42

    val lhs = composed.map(composed.map(nested)(f))(g)
    val rhs = composed.map(nested)(f.andThen(g))

    assert(lhs =!= rhs)
  }

  test("option-like Applicative is not lawful for nested Nullable") {
    val candidate = new Applicative[Nullable] {
      def pure[A](x: A): Nullable[A] = Nullable(x)

      override def map[A, B](fa: Nullable[A])(f: A => B): Nullable[B] =
        fa.transform(f)

      def ap[A, B](ff: Nullable[A => B])(fa: Nullable[A]): Nullable[B] =
        ff.fold(Nullable.empty[B])(f => fa.transform(f))
    }

    val a: Nullable[Int] = Nullable.empty
    val f: Nullable[Int] => Int = _ => 1

    val lhs = candidate.ap(candidate.pure(f))(candidate.pure(a))
    val rhs = candidate.pure(f(a))

    assert(lhs =!= rhs)
  }

  test("option-like Monad is not lawful for nested Nullable") {
    val candidate = new Monad[Nullable] {
      def pure[A](x: A): Nullable[A] = Nullable(x)

      override def map[A, B](fa: Nullable[A])(f: A => B): Nullable[B] =
        fa.transform(f)

      def flatMap[A, B](fa: Nullable[A])(f: A => Nullable[B]): Nullable[B] =
        fa.fold(Nullable.empty[B])(f)

      def tailRecM[A, B](a: A)(f: A => Nullable[Either[A, B]]): Nullable[B] = {
        def loop(current: A): Nullable[B] =
          f(current).fold(Nullable.empty[B]) {
            case Left(next) => loop(next)
            case Right(b)   => Nullable(b)
          }

        loop(a)
      }
    }

    val a: Nullable[Int] = Nullable.empty
    val f: Nullable[Int] => Nullable[Int] = _ => Nullable(1)

    val lhs = candidate.flatMap(candidate.pure(a))(f)
    val rhs = f(a)

    assert(lhs =!= rhs)
  }

  test("current Monoid instance cannot satisfy Group inverse with Int") {
    val x: Nullable[Int] = Nullable(1)
    val inverseX: Nullable[Int] = Nullable(-1)

    assert(Monoid[Nullable[Int]].combine(x, inverseX) =!= Nullable.empty)
  }

  test("Show[Nullable] uses JVM null rendering for null and Show[A] otherwise") {
    val empty: Nullable[Int] = Nullable.empty
    val nonEmpty: Nullable[Int] = Nullable(1)

    assert(Show[Nullable[Int]].show(empty) === java.lang.String.valueOf(null.asInstanceOf[AnyRef]))
    assert(Show[Nullable[Int]].show(nonEmpty) === Show[Int].show(1))
  }

  test("Show[Nullable[Nullable[A]]] is consistent with Show[Nullable[A]]") {
    val nestedEmpty: NestedNullable[Int] = Nullable.empty
    val nestedNonEmpty: NestedNullable[Int] = Nullable(Nullable(1))

    assert(Show[NestedNullable[Int]].show(nestedEmpty) === java.lang.String.valueOf(null.asInstanceOf[AnyRef]))
    assert(Show[NestedNullable[Int]].show(nestedNonEmpty) === Show[Nullable[Int]].show(Nullable(1)))
  }
}
