package cats

import simulacrum._

/**
 * Monad.
 *
 * Allows composition of dependent effectful functions.
 *
 * See: [[http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf Monads for functional programming]]
 *
 * Must obey the following laws:
 *  - flatMap(pure(a))(f) = f(a)
 *  - flatMap(fa)(identity) = fa
 *  - flatMap(flatMap(fa)(f))(g) = flatMap(fa)(a => flatMap(f(a))(g))
 */
trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))
}

object Monad {
  def apply[F[_]](implicit ev: Monad[F]): Monad[F] = ev
}
