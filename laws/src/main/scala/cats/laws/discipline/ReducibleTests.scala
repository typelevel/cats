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
import cats.instances.long._
import cats.kernel.CommutativeMonoid
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop.forAll

trait ReducibleTests[F[_]] extends FoldableTests[F] {
  def laws: ReducibleLaws[F]

  def reducible[G[_]: Applicative, A: Arbitrary, B: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFGA: Arbitrary[F[G[A]]],
    ArbGB: Arbitrary[G[B]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    EqG: Eq[G[Unit]],
    EqA: Eq[A],
    EqB: Eq[B],
    EqFA: Eq[F[A]],
    EqOptionA: Eq[Option[A]],
    MonoidA: CommutativeMonoid[A],
    MonoidB: CommutativeMonoid[B]
  ): RuleSet =
    new DefaultRuleSet(
      name = "reducible",
      parent = Some(foldable[A, B]),
      "reduceLeftTo consistent with reduceMap" -> forAll(laws.reduceLeftToConsistentWithReduceMap[A, B] _),
      "reduceRightTo consistent with reduceMap" -> forAll(laws.reduceRightToConsistentWithReduceMap[A, B] _),
      "reduceRightTo consistent with reduceRightToOption" ->
        forAll(laws.reduceRightToConsistentWithReduceRightToOption[A, B] _),
      "reduceRight consistent with reduceRightOption" ->
        forAll(laws.reduceRightConsistentWithReduceRightOption[A] _),
      "reduce consistent with reduceLeft" ->
        forAll(laws.reduceReduceLeftConsistent[B] _),
      "nonEmptyTraverseVoid consistent with traverseVoid" -> forAll(laws.traverseConsistent[G, A, B] _),
      "nonEmptySequenceVoid consistent with sequenceVoid" -> forAll(laws.sequenceConsistent[G, A] _),
      "size consistent with reduceMap" -> forAll(laws.sizeConsistent[A] _)
    )
}

object ReducibleTests {
  def apply[F[_]: Reducible]: ReducibleTests[F] =
    new ReducibleTests[F] { def laws: ReducibleLaws[F] = ReducibleLaws[F] }
}
