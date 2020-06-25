package cats
package laws
package discipline

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
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

    import cats.kernel.laws._
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
  }

}
