package cats

import simulacrum.typeclass

/**
 * `TraverseEmpty`, also known as `Witherable`, represents list-like structures
 * that can essentially have a `traverse` and a `filter` applied as a single
 * combined operation (`traverseFilter`).
 *
 * Based on Haskell's [[https://hackage.haskell.org/package/witherable-0.1.3.3/docs/Data-Witherable.html Data.Witherable]]
 */

@typeclass
trait TraverseEmpty[F[_]] extends FunctorEmpty[F] {
  def traverse: Traverse[F]

  final override def functor: Functor[F] = traverse

  def traverseFilter[G[_], A, B](fa: F[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[F[B]]

  def filterA[G[_], A](fa: F[A])(f: A => G[Boolean])(implicit G: Applicative[G]): G[F[A]] =
    traverseFilter(fa)(a => G.map(f(a))(if (_) Some(a) else None))

  override def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B] =
    traverseFilter[Id, A, B](fa)(f)
}
