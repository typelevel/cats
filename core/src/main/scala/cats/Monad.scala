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
   * Execute an action repeatedly as long as the given `Boolean` expression
   * returns `true`. The condition is evalated before the loop body.
   * Collects the results into an arbitrary `MonadCombine` value, such as a `List`.
   */
  def whileM[G[_], A](p: F[Boolean])(body: => F[A])(implicit G: MonadCombine[G]): F[G[A]] = {
    lazy val f = body
    ifM(p)(flatMap(f)(x => map(whileM(p)(f))(xs => G.combineK(G.pure(x), xs))), pure(G.empty))
  }

  /**
   * Execute an action repeatedly as long as the given `Boolean` expression
   * returns `true`. The condition is evaluated before the loop body.
   * Discards results.
   */
  def whileM_[A](p: F[Boolean])(body: => F[A]): F[Unit] = {
    lazy val f = body
    ifM(p)(flatMap(f)(_ => whileM_(p)(f)), pure(()))
  }

  /**
   * Execute an action repeatedly until the `Boolean` condition returns `true`.
   * The condition is evaluated after the loop body. Collects results into an
   * arbitrary `MonadCombine` value, such as a `List`.
   */
  def untilM[G[_], A](f: F[A])(cond: => F[Boolean])(implicit G: MonadCombine[G]): F[G[A]] = {
    lazy val p = cond
    flatMap(f)(x => map(whileM(map(p)(!_))(f))(xs => G.combineK(G.pure(x), xs)))
  }

  /**
   * Execute an action repeatedly until the `Boolean` condition returns `true`.
   * The condition is evaluated after the loop body. Discards results.
   */
  def untilM_[A](f: F[A])(cond: => F[Boolean]): F[Unit] = {
    lazy val p = cond
    flatMap(f)(_ => whileM_(map(p)(!_))(f))
  }

  /**
   * Execute an action repeatedly until its result fails to satisfy the given predicate
   * and return that result, discarding all others.
   */
  def iterateWhile[A](f: F[A])(p: A => Boolean): F[A] =
    flatMap(f)(y => if (p(y)) iterateWhile(f)(p) else pure(y))

  /**
   * Execute an action repeatedly until its result satisfies the given predicate
   * and return that result, discarding all others.
   */
  def iterateUntil[A](f: F[A])(p: A => Boolean): F[A] =
    flatMap(f)(y => if (p(y)) pure(y) else iterateUntil(f)(p))

}
