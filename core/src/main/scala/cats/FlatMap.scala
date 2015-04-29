package cats

import simulacrum._

/**
 *  FlatMap typeclass gives us flatMap, which allows us to have a value
 *  in a context (F[A]) and then feed that into a function that takes
 *  a normal value and returns a value in a context (A => F[B]).
 *
 *  One motivation for separating this out from Monad is that there are
 *  situations where we can implement flatMap but not pure.  For example,
 *  we can implement map or flatMap that transforms the values of Map[K, ?],
 *  but we can't implement pure (because we wouldn't know what key to use
 *  when instantiating the new Map).
 *
 *  @see See [[https://github.com/non/cats/issues/3]] for some discussion.
 *
 * Must obey the laws defined in cats.laws.FlatMapLaws.
 */
@typeclass trait FlatMap[F[_]] extends Apply[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  /**
   * also commonly called join
   */
  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    flatMap(ff)(f => map(fa)(f))

  /**
   *  Pair `A` with the result of function application.
   */
  def mproduct[A, B](fa: F[A])(f: A => F[B]): F[(A, B)] =
    flatMap(fa)(a => map(f(a))((a, _)))

  /**
   * `if` lifted into monad.
   */
  def ifM[B](fa: F[Boolean])(ifTrue: => F[B], ifFalse: => F[B]): F[B] = {
    flatMap(fa)(if (_) ifTrue else ifFalse)
  }
}
