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

package cats.free

import cats.{Applicative, Eq, Eval, Functor, Show, Traverse}
import cats.kernel.laws.discipline.{EqTests, HashTests, PartialOrderTests}
import cats.syntax.all.*
import cats.tests.CatsSuite

import org.scalacheck.{Arbitrary, Cogen, Gen}

// this functionality doesn't exist on Scala 2.12
class FreeStructuralSuite extends CatsSuite {
  import FreeSuite.freeArbitrary
  import FreeStructuralSuite.*

  implicit def freeCogen[S[_]: Functor, A](implicit S: => Cogen[S[Free[S, A]]], A: Cogen[A]): Cogen[Free[S, A]] =
    Cogen { (seed, f) =>
      f.resume match {
        case Left(sf) =>
          S.perturb(seed, sf)

        case Right(a) =>
          A.perturb(seed, a)
      }
    }

  Show[Free[Option, Int]]

  checkAll("Free[Option, Int]", HashTests[Free[Option, Int]].hash)
  checkAll("Free[Option, Int]", PartialOrderTests[Free[Option, Int]].partialOrder)
  checkAll("Free[ExprF, String]", EqTests[Free[ExprF, String]].eqv)
}

object FreeStructuralSuite {
  type Expr[A] = Free[ExprF, A]

  // a pattern functor for a simple expression language
  sealed trait ExprF[A] extends Product with Serializable

  object ExprF {
    implicit def eq[A: Eq]: Eq[ExprF[A]] = {
      case (Add(left1, right1), Add(left2, right2)) =>
        left1 === left2 && right1 === right2
      case (Neg(inner1), Neg(inner2)) =>
        inner1 === inner2
      case (Num(value1), Num(value2)) =>
        value1 === value2
      case (_, _) =>
        false
    }

    implicit def traverse: Traverse[ExprF] =
      new Traverse[ExprF] {

        def foldLeft[A, B](fa: ExprF[A], b: B)(f: (B, A) => B): B =
          fa match {
            case Add(left, right) =>
              f(f(b, left), right)

            case Neg(inner) =>
              f(b, inner)

            case Num(_) =>
              b
          }

        def foldRight[A, B](fa: ExprF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
          fa match {
            case Add(left, right) =>
              f(left, f(right, lb))

            case Neg(inner) =>
              f(inner, lb)

            case Num(_) =>
              lb
          }

        def traverse[G[_]: Applicative, A, B](fa: ExprF[A])(f: A => G[B]): G[ExprF[B]] =
          fa match {
            case Add(left, right) =>
              (f(left), f(right)).mapN(Add(_, _))

            case Neg(inner) =>
              f(inner).map(Neg(_))

            case Num(value) =>
              Applicative[G].pure(Num(value))
          }
      }

    implicit def arbitraryExprF[A: Arbitrary](implicit gnum: Arbitrary[Int]): Arbitrary[ExprF[A]] =
      Arbitrary {
        import Arbitrary.arbitrary

        val genAdd: Gen[Add[A]] =
          for {
            left <- arbitrary[A]
            right <- arbitrary[A]
          } yield Add(left, right)

        val genNeg: Gen[Neg[A]] =
          arbitrary[A].map(Neg(_))

        val genNum: Gen[Num[A]] = gnum.arbitrary.map(Num(_))

        Gen.oneOf(genAdd, genNeg, genNum)
      }

    implicit def cogenExprF[A](implicit cg: Cogen[A], cgnum: Cogen[Int]): Cogen[ExprF[A]] =
      Cogen { (seed, ef) =>
        ef match {
          case Add(left, right) =>
            cg.perturb(cg.perturb(seed, left), right)

          case Neg(inner) =>
            cg.perturb(seed, inner)

          case Num(value) =>
            cgnum.perturb(seed, value)
        }
      }

    final case class Add[A](left: A, right: A) extends ExprF[A]
    final case class Neg[A](inner: A) extends ExprF[A]
    final case class Num[A](value: Int) extends ExprF[A]
  }
}
