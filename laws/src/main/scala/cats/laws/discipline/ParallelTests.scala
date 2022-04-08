/*
 * Copyright (c) 2022 Typelevel
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

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

trait ParallelTests[M[_]] extends NonEmptyParallelTests[M] {
  val laws: ParallelLaws[M]

  def parallel[A, B](implicit
    ArbA: Arbitrary[A],
    ArbM: Arbitrary[M[A]],
    ArbMb: Arbitrary[M[B]],
    Arbf: Arbitrary[A => B],
    EqMa: Eq[M[A]],
    EqMb: Eq[M[B]],
    ArbF: Arbitrary[F[A]],
    EqFa: Eq[F[A]]
  ): RuleSet =
    new DefaultRuleSet(
      "parallel",
      Some(nonEmptyParallel[A, B]),
      "isomorphic pure" -> forAll((a: A) => laws.isomorphicPure(a))
    )
}

object ParallelTests {
  type Aux[M[_], F0[_]] = ParallelTests[M] { type F[A] = F0[A]; val laws: ParallelLaws.Aux[M, F0] }

  def apply[M[_]](implicit ev: Parallel[M]): ParallelTests.Aux[M, ev.F] =
    apply[M, ev.F](ev, implicitly)

  def apply[M[_], F0[_]](implicit ev: Parallel.Aux[M, F0], D: DummyImplicit): ParallelTests.Aux[M, F0] =
    new ParallelTests[M] { val laws: ParallelLaws.Aux[M, F0] = ParallelLaws[M](ev) }
}
