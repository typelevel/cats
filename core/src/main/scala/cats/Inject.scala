package cats

import cats.arrow.FunctionK
import cats.data.Coproduct

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

  implicit def catsLeftInjectInstance[F[_], G[_]]: Inject[F, Coproduct[F, G, ?]] =
    new Inject[F, Coproduct[F, G, ?]] {
      val inj = λ[FunctionK[F, Coproduct[F, G, ?]]](Coproduct.leftc(_))

      val prj = λ[FunctionK[Coproduct[F, G, ?], λ[α => Option[F[α]]]]](_.run.swap.toOption)
    }

  implicit def catsRightInjectInstance[F[_], G[_], H[_]](implicit I: Inject[F, G]): Inject[F, Coproduct[H, G, ?]] =
    new Inject[F, Coproduct[H, G, ?]] {
      val inj = λ[FunctionK[G, Coproduct[H, G, ?]]](Coproduct.rightc(_)) compose I.inj

      val prj = λ[FunctionK[Coproduct[H, G, ?], λ[α => Option[F[α]]]]](_.run.toOption.flatMap(I.prj(_)))
    }
}

object Inject extends InjectInstances {
  def apply[F[_], G[_]](implicit I: Inject[F, G]): Inject[F, G] = I
}
