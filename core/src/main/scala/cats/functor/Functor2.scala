package cats
package functor

import simulacrum.typeclass

/**
 * A type class of types which give rise to two independent, covariant
 * functors.
 */
@typeclass trait Functor2[F[_, _]] { self =>

  /**
   * The quintessential method of the Functor2 trait, it applies a
   * function to each "side" of the functor2.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val x: (List[String], Int) = (List("foo", "bar"), 3)
   * scala> x.map2(_.headOption, _.toLong + 1)
   * res0: (Option[String], Long) = (Some(foo),4)
   * }}}
   */
  def map2[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  // derived methods
  /**
   * apply a function to the "left" functor
   */
  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = map2(fab)(f, identity)

  /** The composition of two Functor2s is itself a Functor2 */
  def compose[G[_, _]](implicit G0: Functor2[G]): Functor2[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedFunctor2[F, G] {
      val F = self
      val G = G0
    }

  /**
   * Widens A into a supertype AA.
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> sealed trait Foo
   * scala> case object Bar extends Foo
   * scala> val x1: Either[Bar.type, Int] = Either.left(Bar)
   * scala> val x2: Either[Foo, Int] = x1.leftWiden
   * }}}
   */
  def leftWiden[A, B, AA >: A](fab: F[A, B]): F[AA, B] = fab.asInstanceOf[F[AA, B]]
}

private[cats] trait ComposedFunctor2[F[_, _], G[_, _]]
    extends Functor2[λ[(A, B) => F[G[A, B], G[A, B]]]] {
  def F: Functor2[F]
  def G: Functor2[G]

  override def map2[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] = {
    val innerMap2: G[A, B] => G[C, D] = gab => G.map2(gab)(f, g)
    F.map2(fab)(innerMap2, innerMap2)
  }
}
