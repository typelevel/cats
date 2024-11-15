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

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop.*
import org.typelevel.discipline.Laws

trait SemigroupalTests[F[_]] extends Laws {
  def laws: SemigroupalLaws[F]

  def semigroupal[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    iso: Isomorphisms[F],
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    EqFA: Eq[F[A]],
    EqFABC: Eq[F[(A, B, C)]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "semigroupal",
      parent = None,
      "semigroupal associativity" -> forAll((fa: F[A], fb: F[B], fc: F[C]) =>
        iso.associativity(laws.semigroupalAssociativity(fa, fb, fc))
      )
    )
}

object SemigroupalTests {
  def apply[F[_]: Semigroupal](implicit ev: Isomorphisms[F]): SemigroupalTests[F] =
    new SemigroupalTests[F] { val laws: SemigroupalLaws[F] = SemigroupalLaws[F] }

  trait Isomorphisms[F[_]] {
    def associativity[A, B, C](fs: (F[(A, (B, C))], F[((A, B), C)])): IsEq[F[(A, B, C)]]
    def leftIdentity[A](fs: (F[(Unit, A)], F[A])): IsEq[F[A]]
    def rightIdentity[A](fs: (F[(A, Unit)], F[A])): IsEq[F[A]]
  }

  object Isomorphisms {
    import cats.kernel.laws.*

    implicit def invariant[F[_]](implicit F: Invariant[F]): Isomorphisms[F] =
      new Isomorphisms[F] {
        def associativity[A, B, C](fs: (F[(A, (B, C))], F[((A, B), C)])): IsEq[F[(A, B, C)]] =
          F.imap(fs._1) { case (a, (b, c)) => (a, b, c) } { case (a, b, c) => (a, (b, c)) } <->
            F.imap(fs._2) { case ((a, b), c) => (a, b, c) } { case (a, b, c) => ((a, b), c) }

        def leftIdentity[A](fs: (F[(Unit, A)], F[A])): IsEq[F[A]] =
          F.imap(fs._1) { case (_, a) => a } { a =>
            ((), a)
          } <-> fs._2

        def rightIdentity[A](fs: (F[(A, Unit)], F[A])): IsEq[F[A]] =
          F.imap(fs._1) { case (a, _) => a } { a =>
            (a, ())
          } <-> fs._2
      }

    implicit def composedInvariant[F[_], G[_]](implicit
      F: Invariant[F],
      G: Invariant[G]
    ): Isomorphisms[λ[α => F[G[α]]]] =
      invariant[λ[α => F[G[α]]]](F.compose[G])
  }
}
