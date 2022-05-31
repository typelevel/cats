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

import cats.kernel.CommutativeMonoid
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._
import cats.instances.list._
import arbitrary.catsLawsArbitraryForPartialFunction

trait FoldableTests[F[_]] extends UnorderedFoldableTests[F] {
  def laws: FoldableLaws[F]

  def foldable[A: Arbitrary, B: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    A: CommutativeMonoid[A],
    B: CommutativeMonoid[B],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    EqA: Eq[A],
    EqFA: Eq[F[A]],
    EqB: Eq[B],
    EqOptionB: Eq[Option[B]],
    EqOptionA: Eq[Option[A]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "foldable",
      parent = Some(unorderedFoldable[A, B]),
      "foldLeft consistent with foldMap" -> forAll(laws.leftFoldConsistentWithFoldMap[A, B] _),
      "foldRight consistent with foldMap" -> forAll(laws.rightFoldConsistentWithFoldMap[A, B] _),
      "foldRight is lazy" -> forAll(laws.foldRightLazy[A] _),
      "ordered consistency" -> forAll(laws.orderedConsistency[A] _),
      "exists consistent with find" -> forAll(laws.existsConsistentWithFind[A] _),
      "foldM identity" -> forAll(laws.foldMIdentity[A, B] _),
      "reduceLeftOption consistent with reduceLeftToOption" ->
        forAll(laws.reduceLeftOptionConsistentWithReduceLeftToOption[A] _),
      "reduceRightOption consistent with reduceRightToOption" ->
        forAll(laws.reduceRightOptionConsistentWithReduceRightToOption[A] _),
      "get reference" -> forAll(laws.getRef[A] _),
      "fold reference" -> forAll(laws.foldRef[A] _),
      "toList reference" -> forAll(laws.toListRef[A] _),
      "filter_ reference" -> forAll(laws.filter_Ref[A] _),
      "takeWhile_ reference" -> forAll(laws.takeWhile_Ref[A] _),
      "dropWhile_ reference" -> forAll(laws.dropWhile_Ref[A] _),
      "collectFirstSome reference" -> forAll(laws.collectFirstSome_Ref[A, B] _),
      "collectFirst reference" -> forAll(laws.collectFirst_Ref[A, B] _),
      "foldRightDefer consistency" -> forAll(laws.foldRightDeferConsistentWithFoldRight[A, B] _)
    )
}

object FoldableTests {
  def apply[F[_]: Foldable]: FoldableTests[F] =
    new FoldableTests[F] { def laws: FoldableLaws[F] = FoldableLaws[F] }
}
