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
package laws
package discipline

import cats.instances.option._
import cats.kernel.CommutativeMonoid
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen, Prop}

trait TraverseTests[F[_]] extends FunctorTests[F] with FoldableTests[F] with UnorderedTraverseTests[F] {
  def laws: TraverseLaws[F]

  def traverse[
    A: Arbitrary,
    B: Arbitrary,
    C: Arbitrary,
    M: Arbitrary,
    X[_]: CommutativeApplicative,
    Y[_]: CommutativeApplicative
  ](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbXB: Arbitrary[X[B]],
    ArbXM: Arbitrary[X[M]],
    ArbYB: Arbitrary[Y[B]],
    ArbYC: Arbitrary[Y[C]],
    ArbYM: Arbitrary[Y[M]],
    ArbFXM: Arbitrary[F[X[M]]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenM: Cogen[M],
    M: CommutativeMonoid[M],
    MA: CommutativeMonoid[A],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]],
    EqM: Eq[M],
    EqA: Eq[A],
    EqXYFC: Eq[X[Y[F[C]]]],
    EqXFB: Eq[X[F[B]]],
    EqYFB: Eq[Y[F[B]]],
    EqXFM: Eq[X[F[M]]],
    EqYFM: Eq[Y[F[M]]],
    EqOptionA: Eq[Option[A]]
  ): RuleSet = new RuleSet {
    def name: String = "traverse"
    def bases: Seq[(String, RuleSet)] = Nil
    def parents: Seq[RuleSet] = Seq(functor[A, B, C], foldable[A, M], unorderedTraverse[A, M, C, X, Y])
    def props: Seq[(String, Prop)] =
      Seq(
        "traverse identity" -> forAll(laws.traverseIdentity[A, C] _),
        "traverse sequential composition" -> forAll(laws.traverseSequentialComposition[A, B, C, X, Y] _),
        "traverse parallel composition" -> forAll(laws.traverseParallelComposition[A, B, X, Y] _),
        "traverse traverseTap" -> forAll(laws.traverseTap[B, M, X] _),
        "traverse derive foldMap" -> forAll(laws.foldMapDerived[A, M] _),
        "traverse order consistency" -> forAll(laws.traverseOrderConsistent[A] _),
        "traverse ref mapAccumulate" -> forAll(laws.mapAccumulateRef[M, A, C] _),
        "traverse ref mapWithIndex" -> forAll(laws.mapWithIndexRef[A, C] _),
        "traverse ref traverseWithIndexM" -> forAll(laws.traverseWithIndexMRef[Option, A, C] _),
        "traverse ref zipWithIndex" -> forAll(laws.zipWithIndexRef[A, C] _),
        "traverse ref zipWithLongIndex" -> forAll(laws.zipWithLongIndexRef[A, C] _),
        "traverse ref mapWithLongIndex" -> forAll(laws.mapWithLongIndexRef[A, C] _),
        "traverse ref traverseWithLongIndexM" -> forAll(laws.traverseWithLongIndexMRef[Option, A, C] _),
        "traverse ref updated" -> forAll(laws.updatedRef[A, A](_, _, _))
      )
  }
}

object TraverseTests {
  def apply[F[_]: Traverse]: TraverseTests[F] =
    new TraverseTests[F] { def laws: TraverseLaws[F] = TraverseLaws[F] }
}
