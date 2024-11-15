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

import cats.{Contravariant, ContravariantMonoidal, ContravariantSemigroupal}
import cats.data.Const
import cats.kernel.{Eq, Monoid, Semigroup}
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.discipline.{ContravariantMonoidalTests, ExhaustiveCheck, MiniInt}
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.*

class ContravariantSuite extends CatsSuite {

  test("narrow equals contramap(identity)") {
    implicit val constInst: Contravariant[Const[Int, *]] = Const.catsDataContravariantForConst[Int]
    forAll { (i: Int) =>
      val const: Const[Int, Option[Int]] = Const[Int, Option[Int]](i)
      val narrowed: Const[Int, Some[Int]] = constInst.narrow[Option[Int], Some[Int]](const)
      assert(narrowed === (constInst.contramap(const)(identity[Option[Int]](_: Some[Int]))))
      assert(narrowed eq const)
    }
  }

  case class Predicate[A](run: A => Boolean)

  implicit val contravariantMonoidalPredicate: ContravariantMonoidal[Predicate] =
    new ContravariantMonoidal[Predicate] {
      def unit: Predicate[Unit] = Predicate[Unit](Function.const(true))
      def product[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[(A, B)] =
        Predicate(x => fa.run(x._1) && fb.run(x._2))
      def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] =
        Predicate(x => fa.run(f(x)))
    }

  implicit def eqPredicate[A: ExhaustiveCheck]: Eq[Predicate[A]] =
    Eq.by[Predicate[A], A => Boolean](_.run)

  implicit def arbPredicate[A: Cogen]: Arbitrary[Predicate[A]] =
    Arbitrary(implicitly[Arbitrary[A => Boolean]].arbitrary.map(f => Predicate(f)))

  checkAll("ContravariantMonoidal[Predicate]",
           ContravariantMonoidalTests[Predicate].contravariantMonoidal[Boolean, Boolean, Boolean]
  )

  {
    implicit val predicateMonoid: Monoid[Predicate[MiniInt]] = ContravariantMonoidal.monoid[Predicate, MiniInt]
    checkAll("ContravariantMonoidal[Predicate].monoid", MonoidTests[Predicate[MiniInt]].monoid)
  }
  {
    implicit val predicateSemigroup: Semigroup[Predicate[MiniInt]] =
      ContravariantSemigroupal.semigroup[Predicate, MiniInt]
    checkAll("ContravariantSemigroupal[Predicate].semigroup", SemigroupTests[Predicate[MiniInt]].semigroup)
  }

}
