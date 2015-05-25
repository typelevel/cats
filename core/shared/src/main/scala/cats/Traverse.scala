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

  /**
   * given a function which returns a G effect, thread this effect
   * through the running of this function on all the values in F,
   * returning an F[A] in a G context
   */
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * behaves just like traverse, but uses [[Unapply]] to find the Applicative instace for G
   */
def traverseU[A, GB](fa: F[A])(f: A => GB)(implicit U: Unapply[Applicative, GB]): U.M[F[U.A]] =
    U.TC.traverse(fa)(a => U.subst(f(a)))(this)

  /**
   * thread all the G effects through the F structure to invert the
   * structure from F[G[_]] to G[F[_]]
   */
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  /**
   * behaves just like sequence, but uses [[Unapply]] to find the Applicative instance for G
   */
  def sequenceU[GA](fga: F[GA])(implicit U: Unapply[Applicative,GA]): U.M[F[U.A]] =
    traverse(fga)(U.subst)(U.TC)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)

  def traversal[G[_]: Applicative]: Traversal[G] =
    new Traversal[G]

  class Traversal[G[_]: Applicative] {
    def run[A, B](fa: F[A])(f: A => G[B]): G[F[B]] = traverse[G, A, B](fa)(f)
  }
}
