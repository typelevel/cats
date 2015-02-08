package cats

import simulacrum._

/**
 * Must obey the laws defined in [[laws.CoFlatMapLaws]].
 */
@typeclass trait CoFlatMap[F[_]] extends Functor[F] {
  def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B]

  def coflatten[A](fa: F[A]): F[F[A]] =
    coflatMap(fa)(fa => fa)
}
