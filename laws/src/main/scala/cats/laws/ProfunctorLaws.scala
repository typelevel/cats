package cats
package laws

import cats.functor.Profunctor
import cats.syntax.profunctor._

/**
 * Laws that must be obeyed by any `cats.functor.Profunctor`.
 */
trait ProfunctorLaws[F[_, _]] {
  implicit def F: Profunctor[F]

  def profunctorIdentity[A, B](fab: F[A, B]): IsEq[F[A, B]] =
    fab.dimap(identity[A])(identity[B]) <-> fab

  def profunctorComposition[A2, A1, A0, B0, B1, B2](fab: F[A0, B0],
                                                    f2: A2 => A1, f1: A1 => A0,
                                                    g1: B0 => B1, g2: B1 => B2): IsEq[F[A2, B2]] =
    fab.dimap(f1)(g1).dimap(f2)(g2) <-> fab.dimap(f1 compose f2)(g2 compose g1)
}

object ProfunctorLaws {
  def apply[F[_, _]](implicit ev: Profunctor[F]): ProfunctorLaws[F] =
    new ProfunctorLaws[F] { def F: Profunctor[F] = ev }
}
