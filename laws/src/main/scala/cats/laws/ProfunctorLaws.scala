package cats.laws

import cats.functor.Profunctor
import cats.syntax.profunctor._

/**
 * Laws that must be obeyed by any [[cats.functor.Profunctor]].
 */
class ProfunctorLaws[F[_, _]](implicit F: Profunctor[F]) {
  def profunctorIdentity[A, B](fab: F[A, B]): (F[A, B], F[A, B]) =
    fab.dimap(identity[A])(identity[B]) -> fab

  def profunctorComposition[A2, A1, A0, B0, B1, B2](fab: F[A0, B0],
                                                    f2: A2 => A1, f1: A1 => A0,
                                                    g1: B0 => B1, g2: B1 => B2): (F[A2, B2], F[A2, B2]) =
    fab.dimap(f1)(g1).dimap(f2)(g2) -> fab.dimap(f1 compose f2)(g2 compose g1)
}
