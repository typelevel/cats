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

  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map(fa)(a => (b: B) => (a, b)))(fb)

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
   * Similar to [[map2]] but uses [[Eval]] to allow for laziness in the `F[B]`
   * argument. This can allow for "short-circuiting" of computations.
   *
   * NOTE: the default implementation of `map2Eval` does does not short-circuit
   * computations. For data structures that can benefit from laziness, [[Apply]]
   * instances should override this method.
   *
   * In the following example, `x.map2(bomb)(_ + _)` would result in an error,
   * but `map2Eval` "short-circuits" the computation. `x` is `None` and thus the
   * result of `bomb` doesn't even need to be evaluated in order to determine
   * that the result of `map2Eval` should be `None`.
   *
   * {{{
   * scala> import cats.{Eval, Later}
   * scala> import cats.implicits._
   * scala> val bomb: Eval[Option[Int]] = Later(sys.error("boom"))
   * scala> val x: Option[Int] = None
   * scala> x.map2Eval(bomb)(_ + _).value
   * res0: Option[Int] = None
   * }}}
   */
  def map2Eval[A, B, Z](fa: F[A], fb: Eval[F[B]])(f: (A, B) => Z): Eval[F[Z]] =
    fb.map(fb => map2(fa, fb)(f))

  /**
   * Two sequentially dependent Applys can be composed.
   *
   * The composition of Applys `F` and `G`, `F[G[x]]`, is also an Apply.
   *
   * Example:
   * {{{
   * scala> import cats.Apply
   * scala> import cats.implicits._
   * scala> val ap = Apply[Option].compose[List]
   * scala> val x: Option[List[Int]] = Some(List(1, 2))
   * scala> val y: Option[List[Int]] = Some(List(10, 20))
   * scala> ap.map2(x, y)(_ + _)
   * res0: Option[List[Int]] = Some(List(11, 21, 12, 22))
   * }}}
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

  override def product[A, B](fa: F[G[A]], fb: F[G[B]]): F[G[(A, B)]] =
    F.map2(fa, fb)(G.product)
}
