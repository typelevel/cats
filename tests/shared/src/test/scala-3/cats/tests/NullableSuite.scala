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

import cats.{Applicative, Eq, Eval, Hash, Monad, Monoid, Order, PartialOrder, Semigroup, Show, Traverse}
import cats.data.Nullable
import cats.data.Nullable.*
import cats.kernel.laws.discipline.{EqTests, HashTests, MonoidTests, OrderTests, PartialOrderTests, SemigroupTests}
import cats.laws.discipline.{SerializableTests, TraverseTests}
import cats.syntax.eq.*
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Prop.*

class NullableSuite extends CatsSuite {

  private val optimizedTraverseForNullable: Traverse[Nullable] = Traverse[Nullable]

  private val defaultOnlyTraverse: Traverse[Nullable] = new Traverse[Nullable] {
    override def map[A, B](fa: Nullable[A])(f: A => B): Nullable[B] =
      fa.map(f)

    def foldLeft[A, B](fa: Nullable[A], b: B)(f: (B, A) => B): B =
      fa.fold(b)(a => f(b, a))

    def foldRight[A, B](fa: Nullable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.fold(lb)(a => f(a, lb))

    def traverse[G[_], A, B](fa: Nullable[A])(f: A => G[B])(using G: Applicative[G]): G[Nullable[B]] =
      fa.fold(G.pure(Nullable.empty[B]))(a => G.map(f(a))(Nullable(_)))
  }

  given [A](using A: Arbitrary[A]): Arbitrary[Nullable[A]] =
    Arbitrary(Gen.oneOf(Gen.const(Nullable.empty[A]), A.arbitrary.map(Nullable(_))))

  given [A](using A: Cogen[A]): Cogen[Nullable[A]] =
    Cogen[Option[A]].contramap(_.toOption)

  checkAll("Nullable[Int]", EqTests[Nullable[Int]].eqv)
  checkAll("Eq[Nullable[Int]]", SerializableTests.serializable(Eq[Nullable[Int]]))

  checkAll("Nullable[Int]", HashTests[Nullable[Int]].hash)
  checkAll("Hash[Nullable[Int]]", SerializableTests.serializable(Hash[Nullable[Int]]))

  checkAll("Nullable[Int]", PartialOrderTests[Nullable[Int]].partialOrder)
  checkAll("PartialOrder[Nullable[Int]]", SerializableTests.serializable(PartialOrder[Nullable[Int]]))

  checkAll("Nullable[Int]", OrderTests[Nullable[Int]].order)
  checkAll("Order[Nullable[Int]]", SerializableTests.serializable(Order[Nullable[Int]]))

  checkAll("Nullable[Int]", SemigroupTests[Nullable[Int]].semigroup)
  checkAll("Semigroup[Nullable[Int]]", SerializableTests.serializable(Semigroup[Nullable[Int]]))

  checkAll("Nullable[Int]", MonoidTests[Nullable[Int]].monoid)
  checkAll("Monoid[Nullable[Int]]", SerializableTests.serializable(Monoid[Nullable[Int]]))

  checkAll("Show[Nullable[Int]]", SerializableTests.serializable(Show[Nullable[Int]]))

  checkAll("Nullable[Int] with Option", TraverseTests[Nullable].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Nullable]", SerializableTests.serializable(Traverse[Nullable]))

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

  test("option-like Applicative is not lawful for nested Nullable") {
    val candidate = new Applicative[Nullable] {
      def pure[A](x: A): Nullable[A] = Nullable(x)

      override def map[A, B](fa: Nullable[A])(f: A => B): Nullable[B] =
        fa.map(f)

      def ap[A, B](ff: Nullable[A => B])(fa: Nullable[A]): Nullable[B] =
        ff.fold(Nullable.empty[B])(f => fa.map(f))
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
        fa.map(f)

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

  test("Functor overrides match default implementations") {
    val optimized = optimizedTraverseForNullable
    val defaults = defaultOnlyTraverse

    forAll { (fa: Nullable[Int], b: Int, f: Int => Long, fb: Nullable[Boolean], ifTrue: Int, ifFalse: Int) =>
      assert(optimized.map(fa)(f) === defaults.map(fa)(f))
      assert(optimized.as(fa, b) === defaults.as(fa, b))
      assert(optimized.void(fa) === defaults.void(fa))
      assert(optimized.tupleLeft(fa, b) === defaults.tupleLeft(fa, b))
      assert(optimized.tupleRight(fa, b) === defaults.tupleRight(fa, b))
      assert(optimized.fproduct(fa)(f) === defaults.fproduct(fa)(f))
      assert(optimized.fproductLeft(fa)(f) === defaults.fproductLeft(fa)(f))
      assert(optimized.ifF(fb)(ifTrue, ifFalse) === defaults.ifF(fb)(ifTrue, ifFalse))
    }

    forAll { (fab: Nullable[(Int, String)]) =>
      assert(optimized.unzip(fab) == defaults.unzip(fab))
    }
  }

  test("Foldable overrides match default implementations") {
    val optimized = optimizedTraverseForNullable
    val defaults = defaultOnlyTraverse
    val toLong: Int => Long = _.toLong + 1L
    val sumLong: (Long, Int) => Long = (acc, i) => acc + i.toLong
    val pfEven: PartialFunction[Int, Int] = { case i if i % 2 == 0 => i + 10 }
    val firstSome: Int => Option[Int] = i => if i % 2 == 0 then Some(i + 1) else None
    val traverseVoidF: Int => Option[Int] = i => if i % 2 == 0 then Some(i) else None

    forAll { (fa: Nullable[Int], idx: Long, target: Int, pred: Int => Boolean) =>
      assert(optimized.foldLeft(fa, 1)(_ + _) === defaults.foldLeft(fa, 1)(_ + _))
      assert(
        optimized.foldRight(fa, Eval.now(1))((a, eb) => eb.map(_ + a)).value ===
          defaults.foldRight(fa, Eval.now(1))((a, eb) => eb.map(_ + a)).value
      )
      assert(optimized.fold(fa) === defaults.fold(fa))
      assert(optimized.combineAllOption(fa) === defaults.combineAllOption(fa))
      assert(optimized.toIterable(fa).toList === defaults.toIterable(fa).toList)
      assert(optimized.foldMap(fa)(toLong) === defaults.foldMap(fa)(toLong))
      assert(optimized.reduceLeftToOption(fa)(toLong)(sumLong) === defaults.reduceLeftToOption(fa)(toLong)(sumLong))
      assert(
        optimized.reduceRightToOption(fa)(toLong)((i, e) => e.map(_ + i.toLong)).value ===
          defaults.reduceRightToOption(fa)(toLong)((i, e) => e.map(_ + i.toLong)).value
      )
      assert(optimized.reduceLeftOption(fa)(_ + _) === defaults.reduceLeftOption(fa)(_ + _))
      assert(
        optimized.reduceRightOption(fa)((i, e) => e.map(_ + i)).value ===
          defaults.reduceRightOption(fa)((i, e) => e.map(_ + i)).value
      )
      assert(optimized.minimumOption(fa) === defaults.minimumOption(fa))
      assert(optimized.maximumOption(fa) === defaults.maximumOption(fa))
      assert(optimized.get(fa)(idx) === defaults.get(fa)(idx))
      assert(optimized.contains_(fa, target) === defaults.contains_(fa, target))
      assert(optimized.size(fa) === defaults.size(fa))
      assert(optimized.find(fa)(pred) === defaults.find(fa)(pred))
      assert(optimized.exists(fa)(pred) === defaults.exists(fa)(pred))
      assert(optimized.forall(fa)(pred) === defaults.forall(fa)(pred))
      assert(optimized.toList(fa) === defaults.toList(fa))
      assert(optimized.filter_(fa)(pred) === defaults.filter_(fa)(pred))
      assert(optimized.takeWhile_(fa)(pred) === defaults.takeWhile_(fa)(pred))
      assert(optimized.dropWhile_(fa)(pred) === defaults.dropWhile_(fa)(pred))
      assert(optimized.isEmpty(fa) === defaults.isEmpty(fa))
      assert(optimized.nonEmpty(fa) === defaults.nonEmpty(fa))
      assert(optimized.collectFirst(fa)(pfEven) === defaults.collectFirst(fa)(pfEven))
      assert(optimized.collectFirstSome(fa)(firstSome) === defaults.collectFirstSome(fa)(firstSome))
      assert(optimized.traverseVoid(fa)(traverseVoidF) === defaults.traverseVoid(fa)(traverseVoidF))
    }
  }

  test("Traverse overrides match default implementations") {
    val optimized = optimizedTraverseForNullable
    val defaults = defaultOnlyTraverse

    forAll { (fa: Nullable[Int], fga: Nullable[Option[Int]], init: Long, replacement: Int, idx: Long) =>
      val tapF: Int => Option[String] = i => if i % 2 == 0 then Some(i.toString) else None
      val accumulateF: (Long, Int) => (Long, String) = (s, i) => (s + i.toLong, s"$s:$i")
      val indexTraverseF: (Int, Int) => Option[String] = (i, n) => if (i + n) % 2 == 0 then Some(s"$i-$n") else None
      val longIndexTraverseF: (Int, Long) => Option[String] =
        (i, n) => if (i.toLong + n) % 2L == 0L then Some(s"$i-$n") else None
      val indexMapF: (Int, Int) => String = (i, n) => s"$i@$n"
      val longIndexMapF: (Int, Long) => String = (i, n) => s"$i@$n"

      assert(optimized.traverse(fa)(tapF) === defaults.traverse(fa)(tapF))
      assert(optimized.traverseTap(fa)(tapF) === defaults.traverseTap(fa)(tapF))
      assert(optimized.sequence(fga) === defaults.sequence(fga))
      assert(optimized.mapAccumulate(init, fa)(accumulateF) == defaults.mapAccumulate(init, fa)(accumulateF))
      assert(optimized.mapWithIndex(fa)(indexMapF) === defaults.mapWithIndex(fa)(indexMapF))
      assert(optimized.traverseWithIndexM(fa)(indexTraverseF) === defaults.traverseWithIndexM(fa)(indexTraverseF))
      assert(optimized.zipWithIndex(fa) === defaults.zipWithIndex(fa))
      assert(
        optimized.traverseWithLongIndexM(fa)(longIndexTraverseF) === defaults.traverseWithLongIndexM(fa)(
          longIndexTraverseF
        )
      )
      assert(optimized.mapWithLongIndex(fa)(longIndexMapF) === defaults.mapWithLongIndex(fa)(longIndexMapF))
      assert(optimized.zipWithLongIndex(fa) === defaults.zipWithLongIndex(fa))
      assert(optimized.updated_(fa, idx, replacement) === defaults.updated_(fa, idx, replacement))
    }
  }
}
