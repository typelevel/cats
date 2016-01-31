package cats

import simulacrum.typeclass

/**
 * Weaker version of Applicative[F]; has apply but not pure.
 *
 * Must obey the laws defined in cats.laws.ApplyLaws.
 */
@typeclass(excludeParents=List("ApplyArityFunctions"))
trait Apply[F[_]] extends Functor[F] with Cartesian[F] with ApplyArityFunctions[F] { self =>

  /**
   * Given a value and a function in the Apply context, applies the
   * function to the value.
   */
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  /**
   * ap2 is a binary version of ap, defined in terms of ap.
   */
  def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z] =
    map(product(fa, product(fb, ff))) { case (a, (b, f)) => f(a, b) }

  /**
   * Applies the pure (binary) function f to the effectful values fa and fb.
   *
   * map2 can be seen as a binary version of [[cats.Functor]]#map.
   */
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    map(product(fa, fb)) { case (a, b) => f(a, b) }

  /**
   * Two sequentially dependent Applys can be composed.
   *
   * The composition of Applys `F` and `G`, `F[G[x]]`, is also an Apply.
   *
   * val ap = Apply[Option].compose[List]
   * val x = Some(List(1, 2))
   * val y = Some(List(10, 20))
   * ap.map2(x, y)(_ + _) == Some(List(11, 12, 21, 22))
   */
  def compose[G[_]](implicit GG: Apply[G]): Apply[Lambda[X => F[G[X]]]] =
    new CompositeApply[F, G] {
      def F: Apply[F] = self
      def G: Apply[G] = GG
    }

}

trait CompositeApply[F[_], G[_]]
  extends Apply[Lambda[X => F[G[X]]]] with Functor.Composite[F, G] {
  def F: Apply[F]
  def G: Apply[G]

  def ap[A, B](f: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
    F.ap(F.map(f)(gab => G.ap(gab)(_)))(fa)

  def product[A, B](fa: F[G[A]], fb: F[G[B]]): F[G[(A, B)]] =
    F.map2(fa, fb)(G.product)

}
