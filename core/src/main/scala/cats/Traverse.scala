package cats

import Id._
import simulacrum._

/**
 * Traverse, also known as Traversable.
 *
 * Traversal over a structure with an effect.
 *
 * Traversing with the [[cats.Id]] effect is equivalent to [[cats.Functor]]#map.
 * Traversing with the [[cats.data.Const]] effect where the first type parameter has
 * a [[cats.Monoid]] instance is equivalent to [[cats.Foldable]]#fold.
 *
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 */
@typeclass trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)

  def traversal[G[_]: Applicative]: Traversal[G] =
    new Traversal[G]

  class Traversal[G[_]: Applicative] {
    def run[A, B](fa: F[A])(f: A => G[B]): G[F[B]] = traverse[G, A, B](fa)(f)
  }
}
