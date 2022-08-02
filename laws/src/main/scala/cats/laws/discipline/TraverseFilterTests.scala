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

import cats.data.Nested
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import cats.instances.option._

trait TraverseFilterTests[F[_]] extends FunctorFilterTests[F] {
  def laws: TraverseFilterLaws[F]

  def traverseFilter[A, B, C](implicit
    ArbFA: Arbitrary[F[A]],
    ArbFOA: Arbitrary[F[Option[A]]],
    ArbFABoo: Arbitrary[PartialFunction[A, B]],
    ArbAOB: Arbitrary[A => Option[B]],
    ArbAOA: Arbitrary[A => Option[A]],
    ArbAOOB: Arbitrary[A => Option[Option[B]]],
    ArbBOC: Arbitrary[B => Option[C]],
    ArbBOOC: Arbitrary[B => Option[Option[C]]],
    ArbAB: Arbitrary[A => B],
    ArbABoo: Arbitrary[A => Boolean],
    ArbAOBoo: Arbitrary[A => Option[Boolean]],
    EqFA: Eq[F[A]],
    EqFB: Eq[F[B]],
    EqFC: Eq[F[C]],
    EqGFA: Eq[Option[F[A]]],
    EqMNFC: Eq[Nested[Option, Option, F[C]]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "traverseFilter",
      parent = Some(functorFilter[A, B, C]),
      "traverseFilter identity" -> forAll(laws.traverseFilterIdentity[Option, A] _),
      "traverseFilter nested composition" -> forAll(laws.traverseFilterComposition[A, B, C, Option, Option] _),
      "traverseFilter consistent with traverse" -> forAll(laws.traverseFilterConsistentWithTraverse[Option, A] _),
      "filterA consistent with traverseFilter" -> forAll(laws.filterAConsistentWithTraverseFilter[Option, A] _),
      "traverseEither consistent with traverseFilter" -> forAll(
        laws.traverseEitherConsistentWithTraverseFilter[Option, F[A], A, B] _
      ),
      "traverseFilter ref traverseCollect" -> forAll(laws.traverseCollectRef[Option, A, B] _)
    )
}

object TraverseFilterTests {
  def apply[F[_]: TraverseFilter]: TraverseFilterTests[F] =
    new TraverseFilterTests[F] { def laws: TraverseFilterLaws[F] = TraverseFilterLaws[F] }
}
