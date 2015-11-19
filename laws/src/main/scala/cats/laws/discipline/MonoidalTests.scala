package cats
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

trait MonoidalTests[F[_]] extends Laws {
  def laws: MonoidalLaws[F]

  def monoidal[A : Arbitrary, B : Arbitrary, C : Arbitrary](implicit
    iso: MonoidalTests.Isomorphisms[F],
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbFC: Arbitrary[F[C]],
    ArbFUnit: Arbitrary[F[Unit]],
    EqFA: Eq[F[A]],
    EqFABC: Eq[F[(A, B, C)]]
  ): RuleSet = {
    new DefaultRuleSet(
      name = "monoidal",
      parent = None,
      "associativity" -> forAll((fa: F[A], fb: F[B], fc: F[C]) => iso.`((a, b), c) ≅ (a, (b, c))`(laws.associativity(fa, fb, fc))),
      "left identity" -> forAll((fa: F[A], funit: F[Unit]) => iso.`(unit, a) ≅ a`(laws.leftIdentity(funit, fa))),
      "right identity" -> forAll((fa: F[A], funit: F[Unit]) => iso.`(a, unit) ≅ a`(laws.rightIdentity(fa, funit)))
    )
  }
}

object MonoidalTests {
  def apply[F[_] : Monoidal](implicit ev: Isomorphisms[F]): MonoidalTests[F] =
    new MonoidalTests[F] { val laws: MonoidalLaws[F] = MonoidalLaws[F] }

  trait Isomorphisms[F[_]] {
    def `((a, b), c) ≅ (a, (b, c))`[A, B, C](fs: (F[(A, (B, C))], F[((A, B), C)]))(implicit EqFABC: Eq[F[(A, B, C)]]): Prop
    def `(unit, a) ≅ a`[A](fs: (F[(Unit, A)], F[A]))(implicit EqFA: Eq[F[A]]): Prop
    def `(a, unit) ≅ a`[A](fs: (F[(A, Unit)], F[A]))(implicit EqFA: Eq[F[A]]): Prop
  }

  object Isomorphisms {
    import algebra.laws._
    implicit def covariant[F[_]](implicit F: Functor[F]): Isomorphisms[F] =
      new Isomorphisms[F] {
        def `((a, b), c) ≅ (a, (b, c))`[A, B, C](fs: (F[(A, (B, C))], F[((A, B), C)]))(implicit EqFABC: Eq[F[(A, B, C)]]) =
          F.map(fs._1) { case (a, (b, c)) => (a, b, c) } ?== F.map(fs._2) { case ((a, b), c) => (a, b, c) }
        def `(unit, a) ≅ a`[A](fs: (F[(Unit, A)], F[A]))(implicit EqFA: Eq[F[A]]) =
          F.map(fs._1)(_._2) ?== fs._2
        def `(a, unit) ≅ a`[A](fs: (F[(A, Unit)], F[A]))(implicit EqFA: Eq[F[A]]) =
          F.map(fs._1)(_._1) ?== fs._2
      }
    implicit def contravariant[F[_]](implicit F: functor.Contravariant[F]): Isomorphisms[F] =
      new Isomorphisms[F] {
        def `((a, b), c) ≅ (a, (b, c))`[A, B, C](fs: (F[(A, (B, C))], F[((A, B), C)]))(implicit EqFABC: Eq[F[(A, B, C)]]) =
          F.contramap[(A, (B, C)), (A, B, C)](fs._1) { case (a, b, c) => (a, (b, c)) } ?== F.contramap[((A, B), C), (A, B, C)](fs._2) { case (a, b, c) => ((a, b), c) }
        def `(unit, a) ≅ a`[A](fs: (F[(Unit, A)], F[A]))(implicit EqFA: Eq[F[A]]) =
          F.contramap(fs._1)((a: A) => ((), a)) ?== fs._2
        def `(a, unit) ≅ a`[A](fs: (F[(A, Unit)], F[A]))(implicit EqFA: Eq[F[A]]) =
          F.contramap(fs._1)((a: A) => (a, ())) ?== fs._2
      }
  }

}