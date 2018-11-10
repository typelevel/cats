package cats
package laws
package discipline

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait DecideableTests[F[_]] extends ContravariantMonoidalTests[F] {
  def laws: DecideableLaws[F]

  def decideable[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
                                                           arbFA: Arbitrary[F[A]],
                                                           arbFB: Arbitrary[F[B]],
                                                           arbFC: Arbitrary[F[C]],
                                                           CogenA: Cogen[A],
                                                           CogenB: Cogen[B],
                                                           CogenC: Cogen[C],
                                                           EqFA: Eq[F[A]],
                                                           EqFB: Eq[F[B]],
                                                           EqFC: Eq[F[C]],
                                                           EqFABC: Eq[F[(A, B, C)]],
                                                           EqFEitABC: Eq[F[Either[Either[A, B], C]]],
                                                           iso: SemigroupalTests.Isomorphisms[F],
                                                           iso2: DecideableTests.Isomorphisms[F]): RuleSet =
    new RuleSet {
      val name = "decideable"
      val parents = Seq(contravariantMonoidal[A, B, C])
      val bases = Seq.empty
      val props = Seq(
        "decideable right absorption" ->
          forAll(laws.decideableDecideRightAbsorption[A] _),
        "decideable sum associativity" -> forAll(
          (fa: F[A], fb: F[B], fc: F[C]) => iso2.associativity(laws.decideableSumAssociativity[A, B, C](fa, fb, fc))
        )
      )
    }
}

object DecideableTests {
  def apply[F[_]: Decideable](implicit ev: Isomorphisms[F]): DecideableTests[F] =
    new DecideableTests[F] { def laws: DecideableLaws[F] = DecideableLaws[F] }

  trait Isomorphisms[F[_]] {
    def associativity[A, B, C](
      fs: (F[Either[A, Either[B, C]]], F[Either[Either[A, B], C]])
    ): IsEq[F[Either[Either[A, B], C]]]
    def leftIdentity[A](fs: (F[Either[Nothing, A]], F[A])): IsEq[F[A]]
    def rightIdentity[A](fs: (F[Either[A, Nothing]], F[A])): IsEq[F[A]]
  }

  object Isomorphisms {
    private def absurd[A](n: Nothing): A = n.asInstanceOf[A]

    import cats.kernel.laws._
    implicit def invariant[F[_]](implicit F: Invariant[F]): Isomorphisms[F] =
      new Isomorphisms[F] {
        def associativity[A, B, C](
          fs: (F[Either[A, Either[B, C]]], F[Either[Either[A, B], C]])
        ): IsEq[F[Either[Either[A, B], C]]] =
          F.imap(fs._1) {
            case Left(a)         => Left(Left(a))
            case Right(Left(b))  => Left(Right(b))
            case Right(Right(c)) => Right(c)
          } {
            case Left(Left(a))  => Left(a)
            case Left(Right(b)) => Right(Left(b))
            case Right(c)       => Right(Right(c))
          } <-> fs._2

        def leftIdentity[A](fs: (F[Either[Nothing, A]], F[A])): IsEq[F[A]] =
          F.imap(fs._1) { case Right(a) => a; case Left(n) => absurd[A](n) } { (a: A) =>
            Right(a)
          } <-> fs._2

        def rightIdentity[A](fs: (F[Either[A, Nothing]], F[A])): IsEq[F[A]] =
          F.imap(fs._1) { case Left(a) => a; case Right(n) => absurd[A](n) } { (a: A) =>
            Left(a)
          } <-> fs._2
      }
  }
}
