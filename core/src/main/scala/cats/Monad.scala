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
   * returns `true`. The condition is evaluated before the loop body.
   * Collects the results into an arbitrary `Alternative` value, such as a `Vector`.
   * This implementation uses append on each evaluation result,
   * so avoid data structures with non-constant append performance, e.g. `List`.
  */
  def whileM[G[_], A](p: F[Boolean])(body: => F[A])(implicit G: Alternative[G]): F[G[A]] = {
    val b = Eval.later(body)
    tailRecM[G[A], G[A]](G.empty)(xs => ifM(p)(
      ifTrue = {
        map(b.value) { bv =>
          Left(G.combineK(xs, G.pure(bv)))
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
    val b = Eval.later(body)
    tailRecM(())(_ => ifM(p)(
      ifTrue = {
        map(b.value)(_ => continue)
      },
      ifFalse = stop
    ))
  }

  /**
   * Execute an action repeatedly until the `Boolean` condition returns `true`.
   * The condition is evaluated after the loop body. Collects results into an
   * arbitrary `Alternative` value, such as a `Vector`.
   * This implementation uses append on each evaluation result,
   * so avoid data structures with non-constant append performance, e.g. `List`.
   */
  def untilM[G[_], A](f: F[A])(cond: => F[Boolean])(implicit G: Alternative[G]): F[G[A]] = {
    val p = Eval.later(cond)
    flatMap(f)(x => map(whileM(map(p.value)(!_))(f))(xs => G.combineK(G.pure(x), xs)))
  }

  /**
   * Execute an action repeatedly until the `Boolean` condition returns `true`.
   * The condition is evaluated after the loop body. Discards results.
   */
  def untilM_[A](f: F[A])(cond: => F[Boolean]): F[Unit] = {
    val p = Eval.later(cond)
    flatMap(f)(_ => whileM_(map(p.value)(!_))(f))
  }

  /**
   * Execute an action repeatedly until its result fails to satisfy the given predicate
   * and return that result, discarding all others.
   */
  def iterateWhile[A](f: F[A])(p: A => Boolean): F[A] = {
    flatMap(f) { i =>
      tailRecM(i) { a =>
        if (p(a))
          map(f)(_.asLeft[A])
        else pure(a.asRight[A])
      }
    }
  }

  /**
   * Execute an action repeatedly until its result satisfies the given predicate
   * and return that result, discarding all others.
   */
  def iterateUntil[A](f: F[A])(p: A => Boolean): F[A] = {
    flatMap(f) { i =>
      tailRecM(i) { a =>
        if (p(a))
          pure(a.asRight[A])
        else map(f)(_.asLeft[A])
      }
    }
  }

}
