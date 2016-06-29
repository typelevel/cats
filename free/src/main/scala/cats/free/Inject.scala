package cats.free

import cats.Functor
import cats.data.Coproduct


/**
 * Inject type class as described in "Data types a la carte" (Swierstra 2008).
 *
 * @see [[http://www.staff.science.uu.nl/~swier004/publications/2008-jfp.pdf]]
 */
sealed abstract class Inject[F[_], G[_]] {
  def inj[A](fa: F[A]): G[A]

  def prj[A](ga: G[A]): Option[F[A]]
}

private[free] sealed abstract class InjectInstances {
  implicit def catsFreeReflexiveInjectInstance[F[_]]: Inject[F, F] =
    new Inject[F, F] {
      def inj[A](fa: F[A]): F[A] = fa

      def prj[A](ga: F[A]): Option[F[A]] = Some(ga)
    }

  implicit def catsFreeLeftInjectInstance[F[_], G[_]]: Inject[F, Coproduct[F, G, ?]] =
    new Inject[F, Coproduct[F, G, ?]] {
      def inj[A](fa: F[A]): Coproduct[F, G, A] = Coproduct.leftc(fa)

      def prj[A](ga: Coproduct[F, G, A]): Option[F[A]] = ga.run.fold(Some(_), _ => None)
    }

  implicit def catsFreeRightInjectInstance[F[_], G[_], H[_]](implicit I: Inject[F, G]): Inject[F, Coproduct[H, G, ?]] =
    new Inject[F, Coproduct[H, G, ?]] {
      def inj[A](fa: F[A]): Coproduct[H, G, A] = Coproduct.rightc(I.inj(fa))

      def prj[A](ga: Coproduct[H, G, A]): Option[F[A]] = ga.run.fold(_ => None, I.prj(_))
    }
}

object Inject extends InjectInstances {
  def inject[F[_], G[_], A](ga: G[Free[F, A]])(implicit I: Inject[G, F]): Free[F, A] =
    Free.liftF(I.inj(ga)) flatMap identity

  def match_[F[_], G[_], A](fa: Free[F, A])(implicit F: Functor[F], I: Inject[G, F]): Option[G[Free[F, A]]] =
    fa.resume.fold(I.prj, _ => None)

  def apply[F[_], G[_]](implicit I: Inject[F, G]): Inject[F, G] = I
}
