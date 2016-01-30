package cats

import simulacrum.typeclass

/**
 * Monad.
 *
 * Allows composition of dependent effectful functions.
 *
 * See: [[http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf Monads for functional programming]]
 *
 * Must obey the laws defined in cats.laws.MonadLaws.
 */
@typeclass trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  /**
   * Lift a value of type M[A] into a monad transformer MT[M, A]
   */
  def liftM[MT[_[_], _]](ma: M[A])(implicit MT: MonadTrans[MT]): MT[F, A] = MT.liftM(ma)
}
