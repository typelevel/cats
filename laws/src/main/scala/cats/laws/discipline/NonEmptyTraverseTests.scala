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

package cats.laws.discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop.forAll
import cats.kernel.CommutativeMonoid
import cats.{Applicative, CommutativeApplicative, Eq, NonEmptyTraverse}
import cats.laws.NonEmptyTraverseLaws

trait NonEmptyTraverseTests[F[_]] extends TraverseTests[F] with ReducibleTests[F] {
  def laws: NonEmptyTraverseLaws[F]

  def nonEmptyTraverse[
    G[_]: Applicative,
    A: Arbitrary,
    B: Arbitrary,
    C: Arbitrary,
    M: Arbitrary,
    X[_],
    Y[_]
  ](implicit
    ArbFA: Arbitrary[F[A]],
    ArbXB: Arbitrary[X[B]],
    ArbYB: Arbitrary[Y[B]],
    ArbYC: Arbitrary[Y[C]],
    ArbFB: Arbitrary[F[B]],
    ArbFM: Arbitrary[F[M]],
    ArbXM: Arbitrary[X[M]],
    ArbYM: Arbitrary[Y[M]],
    ArbFGA: Arbitrary[F[G[A]]],
    ArbFXM: Arbitrary[F[X[M]]],
    ArbGB: Arbitrary[G[B]],
    ArbGM: Arbitrary[G[M]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenM: Cogen[M],
    M: CommutativeMonoid[M],
    MA: CommutativeMonoid[A],
    MB: CommutativeMonoid[B],
    CX: CommutativeApplicative[X],
    CY: CommutativeApplicative[Y],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]],
    EqG: Eq[G[Unit]],
    EqM: Eq[M],
    EqA: Eq[A],
    EqB: Eq[B],
    EqXYFC: Eq[X[Y[F[C]]]],
    EqXFB: Eq[X[F[B]]],
    EqXFM: Eq[X[F[M]]],
    EqYFB: Eq[Y[F[B]]],
    EqYFM: Eq[Y[F[M]]],
    EqOptionA: Eq[Option[A]]
  ): RuleSet = new RuleSet {
    def name: String = "nonEmptyTraverse"
    def bases: Seq[(String, RuleSet)] = Nil
    def parents: Seq[RuleSet] = Seq(traverse[A, B, C, M, X, Y], reducible[G, A, B])
    def props: Seq[(String, Prop)] =
      Seq(
        "nonEmptyTraverse identity" -> forAll(laws.nonEmptyTraverseIdentity[A, C] _),
        "nonEmptyTraverse sequential composition" -> forAll(
          laws.nonEmptyTraverseSequentialComposition[A, B, C, X, Y] _
        ),
        "nonEmptyTraverse parallel composition" -> forAll(laws.nonEmptyTraverseParallelComposition[A, B, X, Y] _),
        "nonEmptyTraverse derive reduceMap" -> forAll(laws.reduceMapDerived[A, M] _)
      )
  }
}

object NonEmptyTraverseTests {
  def apply[F[_]: NonEmptyTraverse]: NonEmptyTraverseTests[F] =
    new NonEmptyTraverseTests[F] { def laws: NonEmptyTraverseLaws[F] = NonEmptyTraverseLaws[F] }
}
