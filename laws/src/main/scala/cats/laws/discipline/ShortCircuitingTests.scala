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

import cats.laws.ShortCircuitingLaws
import cats.{Eq, Foldable, NonEmptyTraverse, Traverse, TraverseFilter}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait ShortCircuitingTests[F[_]] extends Laws {
  def laws: ShortCircuitingLaws[F]

  def foldable[A: Arbitrary](implicit F: Foldable[F], ArbFA: Arbitrary[F[A]], lEq: Eq[Long]): RuleSet =
    new DefaultRuleSet(
      name = "foldMapKShortCircuiting",
      parent = None,
      "foldMapK short-circuits if MonoidK[G].combineKEval shorts" -> forAll(laws.foldMapKShortCircuits[A] _),
      "foldMapK won't short-circuit if MonoidK[G].combineKEval won't" -> forAll(laws.foldMapKWontShortCircuit[A] _)
    )

  def traverse[A: Arbitrary](implicit F: Traverse[F], ArbFA: Arbitrary[F[A]], lEq: Eq[Long]): RuleSet =
    new DefaultRuleSet(
      name = "traverseShortCircuiting",
      parent = None,
      "traverse short-circuits if Applicative[G].map2Eval shorts" -> forAll(laws.traverseShortCircuits[A] _),
      "traverse won't short-circuit if Applicative[G].map2Eval won't" -> forAll(laws.traverseWontShortCircuit[A] _)
    )

  def nonEmptyTraverse[A: Arbitrary](implicit TF: NonEmptyTraverse[F], ArbFA: Arbitrary[F[A]], lEq: Eq[Long]): RuleSet =
    new DefaultRuleSet(
      name = "nonEmptyTraverseShortCircuiting",
      parent = Some(traverse[A]),
      "nonEmptyTraverse short-circuits if Applicative[G].map2Eval shorts" ->
        forAll(laws.nonEmptyTraverseShortCircuits[A] _),
      "nonEmptyTraverse short-circuits if Applicative[G].map2Eval won't" ->
        forAll(laws.nonEmptyTraverseWontShortCircuit[A] _)
    )

  def traverseFilter[A: Arbitrary](implicit TF: TraverseFilter[F], ArbFA: Arbitrary[F[A]], lEq: Eq[Long]): RuleSet = {
    implicit val T: Traverse[F] = TF.traverse
    new DefaultRuleSet(
      name = "traverseFilterShortCircuiting",
      parent = Some(traverse[A]),
      "traverseFilter short-circuits if Applicative[G].map2Eval shorts" ->
        forAll(laws.traverseFilterShortCircuits[A] _),
      "traverseFilter short-circuits if Applicative[G].map2Eval won't" ->
        forAll(laws.traverseFilterWontShortCircuit[A] _),
      "filterA short-circuits if Applicative[G].map2Eval shorts" -> forAll(laws.filterAShortCircuits[A] _),
      "filterA short-circuits if Applicative[G].map2Eval won't" -> forAll(laws.filterAWontShortCircuit[A] _)
    )
  }
}

object ShortCircuitingTests {
  def apply[F[_]]: ShortCircuitingTests[F] =
    new ShortCircuitingTests[F] {
      override def laws: ShortCircuitingLaws[F] = ShortCircuitingLaws[F]
    }
}
