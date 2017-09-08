package cats

import cats.arrow.FunctionK
import cats.data.EitherK

/**
  * InjectK is a type class providing an injection from type
  * constructor `F` into type constructor `G`. An injection is a
  * functor transformation `inj` which does not destroy any
  * information: for every `ga: G[A]` there is at most one `fa: F[A]`
  * such that `inj(fa) = ga`.
  *
  * Because of this all injections admit partial inverses `prj` which
  * pair a value `ga: G[A]` back with a single value `fa: F[A]`.
  *
  * The behavior of the default instances for the InjectK type class
  * are described thoroughly in "Data types a la carte" (Swierstra
  * 2008).
  *
  * @note Prior to cats 1.0, InjectK was known as [[Inject]].
  *
  * @see [[http://www.staff.science.uu.nl/~swier004/publications/2008-jfp.pdf]]
  * @see [[Inject]] for injection for `Either`
  */
abstract class InjectK[F[_], G[_]] {
  def inj: FunctionK[F, G]

  def prj: FunctionK[G, λ[α => Option[F[α]]]]

  final def apply[A](fa: F[A]): G[A] = inj(fa)

  final def unapply[A](ga: G[A]): Option[F[A]] = prj(ga)
}

private[cats] sealed abstract class InjectKInstances {
  implicit def catsReflexiveInjectKInstance[F[_]]: InjectK[F, F] =
    new InjectK[F, F] {
      val inj = FunctionK.id[F]

      val prj = λ[FunctionK[F, λ[α => Option[F[α]]]]](Some(_))
    }

  implicit def catsLeftInjectKInstance[F[_], G[_]]: InjectK[F, EitherK[F, G, ?]] =
    new InjectK[F, EitherK[F, G, ?]] {
      val inj = λ[FunctionK[F, EitherK[F, G, ?]]](EitherK.leftc(_))

      val prj = λ[FunctionK[EitherK[F, G, ?], λ[α => Option[F[α]]]]](_.run.left.toOption)
    }

  implicit def catsRightInjectKInstance[F[_], G[_], H[_]](implicit I: InjectK[F, G]): InjectK[F, EitherK[H, G, ?]] =
    new InjectK[F, EitherK[H, G, ?]] {
      val inj = λ[FunctionK[G, EitherK[H, G, ?]]](EitherK.rightc(_)) compose I.inj

      val prj = λ[FunctionK[EitherK[H, G, ?], λ[α => Option[F[α]]]]](_.run.right.toOption.flatMap(I.prj(_)))
    }
}

object InjectK extends InjectKInstances {
  def apply[F[_], G[_]](implicit I: InjectK[F, G]): InjectK[F, G] = I
}
