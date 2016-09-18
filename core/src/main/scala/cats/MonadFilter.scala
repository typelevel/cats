package cats

import simulacrum.typeclass

/**
 * a Monad equipped with an additional method which allows us to
 * create an "empty" value for the Monad (for whatever "empty" makes
 * sense for that particular monad). This is of particular interest to
 * us since it allows us to add a `filter` method to a Monad, which is
 * used when pattern matching or using guards in for comprehensions.
 */
@typeclass trait MonadFilter[F[_]] extends FunctorFilter[F] {
  def monadInstance: Monad[F]
  def functorInstance: Functor[F] = monadInstance

  def empty[A]: F[A]

  override def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B] =
    monadInstance.flatMap(fa)(a => f(a).fold(empty[B])(monadInstance.pure))
}
