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
}

object Monad {
  /**
   * Sometimes we only require an Applicative, but if we have a Monad we could
   * do some optimization (for instance, using tailRecM). This method optionally
   * gives that.
   */
  private[cats] def maybeFromApplicative[F[_]](implicit a: Applicative[F]): Option[Monad[F]] = a match {
    case m: Monad[F] => Some(m) // Since Monad extends Applicative, the only value for the type is F
    case _ => None
  }
}
