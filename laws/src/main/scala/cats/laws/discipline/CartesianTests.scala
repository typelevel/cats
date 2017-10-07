package cats
package laws
package discipline

import cats.laws.discipline.CartesianTests.Isomorphisms
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

trait CartesianTests[F[_]] extends Laws {
  def laws: CartesianLaws[F]

  def cartesian[A : Arbitrary, B : Arbitrary, C : Arbitrary](implicit
    iso: Isomorphisms[F],
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    EqFA: Eq[F[A]],
    EqFABC: Eq[F[(A, B, C)]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "cartesian",
      parent = None,
      "cartesian associativity" -> forAll((fa: F[A], fb: F[B], fc: F[C]) => iso.associativity(laws.cartesianAssociativity(fa, fb, fc)))
    )
  }
}

object CartesianTests {
  def apply[F[_] : Cartesian](implicit ev: Isomorphisms[F]): CartesianTests[F] =
    new CartesianTests[F] { val laws: CartesianLaws[F] = CartesianLaws[F] }

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
          F.imap(fs._1) { case (_, a) => a } { a => ((), a) } <-> fs._2

        def rightIdentity[A](fs: (F[(A, Unit)], F[A])): IsEq[F[A]] =
          F.imap(fs._1) { case (a, _) => a } { a => (a, ()) } <-> fs._2
      }
  }

}
