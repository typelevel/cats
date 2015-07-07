package cats

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.CoflatMapLaws.
 */
@typeclass trait CoflatMap[F[_]] extends Functor[F] {
  def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B]

  def coflatten[A](fa: F[A]): F[F[A]] =
    coflatMap(fa)(fa => fa)
}
