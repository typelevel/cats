package cats

import simulacrum.typeclass
import syntax.either._
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
   * Execute an action repeatedly as long as the given `Boolean` expression
   * returns `true`. The condition is evalated before the loop body.
   * Collects the results into an arbitrary `MonadCombine` value, such as a `List`.
  */
  def whileM[G[_], A](p: F[Boolean])(body: => F[A])(implicit G: MonadCombine[G]): F[G[A]] = {
    tailRecM[G[A], G[A]](G.empty)(xs => ifM(p)(
      ifTrue = {
        map(body) { b =>
          Left(G.combineK(G.pure(b), xs))
        }
      },
      ifFalse = pure(xs.asRight[G[A]])
    ))
  }

  /**
   * Execute an action repeatedly as long as the given `Boolean` expression
   * returns `true`. The condition is evaluated before the loop body.
   * Discards results.
   */
  def whileM_[A](p: F[Boolean])(body: => F[A]): F[Unit] = {
    val continue: Either[Unit, Unit] = Left(())
    val stop: F[Either[Unit, Unit]] = pure(Right(()))
    tailRecM(())(_ => ifM(p)(
      ifTrue = {
        map(body)(_ => continue)
      },
      ifFalse = stop
    ))
  }
}
