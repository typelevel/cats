package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

trait CartesianTests[F[_]] extends Laws {
  def laws: CartesianLaws[F]

  def cartesian[A : Arbitrary, B : Arbitrary, C : Arbitrary](implicit
    iso: CartesianTests.Isomorphisms[F],
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
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
    def associativity[A, B, C](fs: (F[(A, (B, C))], F[((A, B), C)]))(implicit EqFABC: Eq[F[(A, B, C)]]): Prop
    def leftIdentity[A](fs: (F[(Unit, A)], F[A]))(implicit EqFA: Eq[F[A]]): Prop
    def rightIdentity[A](fs: (F[(A, Unit)], F[A]))(implicit EqFA: Eq[F[A]]): Prop
  }

  object Isomorphisms {
    import cats.kernel.laws._
    implicit def invariant[F[_]](implicit F: functor.Invariant[F]): Isomorphisms[F] =
      new Isomorphisms[F] {
        def associativity[A, B, C](fs: (F[(A, (B, C))], F[((A, B), C)]))(implicit EqFABC: Eq[F[(A, B, C)]]) =
          F.imap(fs._1) { case (a, (b, c)) => (a, b, c) } { case (a, b, c) => (a, (b, c)) } ?==
          F.imap(fs._2) { case ((a, b), c) => (a, b, c) } { case (a, b, c) => ((a, b), c) }

        def leftIdentity[A](fs: (F[(Unit, A)], F[A]))(implicit EqFA: Eq[F[A]]): Prop =
          F.imap(fs._1) { case (_, a) => a } { a => ((), a) } ?== fs._2

        def rightIdentity[A](fs: (F[(A, Unit)], F[A]))(implicit EqFA: Eq[F[A]]): Prop =
          F.imap(fs._1) { case (a, _) => a } { a => (a, ()) } ?== fs._2
      }
  }

}
