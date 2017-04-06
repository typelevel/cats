package cats

import cats.arrow.FunctionK
import cats.data.EitherK

/**
 * Inject type class as described in "Data types a la carte" (Swierstra 2008).
 *
 * @see [[http://www.staff.science.uu.nl/~swier004/publications/2008-jfp.pdf]]
 */
sealed abstract class Inject[F[_], G[_]] {
  def inj: FunctionK[F, G]

  def prj: FunctionK[G, λ[α => Option[F[α]]]]

  def apply[A](fa: F[A]): G[A] = inj(fa)

  def unapply[A](ga: G[A]): Option[F[A]] = prj(ga)
}

private[cats] sealed abstract class InjectInstances {
  implicit def catsReflexiveInjectInstance[F[_]]: Inject[F, F] =
    new Inject[F, F] {
      val inj = λ[FunctionK[F, F]](identity(_))

      val prj = λ[FunctionK[F, λ[α => Option[F[α]]]]](Some(_))
    }

  implicit def catsLeftInjectInstance[F[_], G[_]]: Inject[F, EitherK[F, G, ?]] =
    new Inject[F, EitherK[F, G, ?]] {
      val inj = λ[FunctionK[F, EitherK[F, G, ?]]](EitherK.leftc(_))

      val prj = λ[FunctionK[EitherK[F, G, ?], λ[α => Option[F[α]]]]](_.run.left.toOption)
    }

  implicit def catsRightInjectInstance[F[_], G[_], H[_]](implicit I: Inject[F, G]): Inject[F, EitherK[H, G, ?]] =
    new Inject[F, EitherK[H, G, ?]] {
      val inj = λ[FunctionK[G, EitherK[H, G, ?]]](EitherK.rightc(_)) compose I.inj

      val prj = λ[FunctionK[EitherK[H, G, ?], λ[α => Option[F[α]]]]](_.run.right.toOption.flatMap(I.prj(_)))
    }
}

object Inject extends InjectInstances {
  def apply[F[_], G[_]](implicit I: Inject[F, G]): Inject[F, G] = I
}
