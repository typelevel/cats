package cats

import simulacrum.typeclass

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

  /**
   * Given a function which returns a G effect, thread this effect
   * through the running of this function on all the values in F,
   * returning an F[B] in a G context.
   */
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * Behaves just like traverse, but uses [[Unapply]] to find the
   * Applicative instance for G.
   */
  def traverseU[A, GB](fa: F[A])(f: A => GB)(implicit U: Unapply[Applicative, GB]): U.M[F[U.A]] =
    U.TC.traverse(fa)(a => U.subst(f(a)))(this)

  /**
   * A traverse followed by flattening the inner result.
   */
  def traverseM[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: FlatMap[F]): G[F[B]] =
    G.map(traverse(fa)(f))(F.flatten)

  /**
   * Thread all the G effects through the F structure to invert the
   * structure from F[G[A]] to G[F[A]].
   */
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  /**
   * Behaves just like sequence, but uses [[Unapply]] to find the
   * Applicative instance for G.
   */
  def sequenceU[GA](fga: F[GA])(implicit U: Unapply[Applicative,GA]): U.M[F[U.A]] =
    traverse(fga)(U.subst)(U.TC)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)
}
