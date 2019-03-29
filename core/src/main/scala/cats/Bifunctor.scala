package cats
import simulacrum.typeclass

/**
 * A type class of types which give rise to two independent, covariant
 * functors.
 */
@typeclass trait Bifunctor[F[_, _]] { self =>

  /**
   * The quintessential method of the Bifunctor trait, it applies a
   * function to each "side" of the bifunctor.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val x: (List[String], Int) = (List("foo", "bar"), 3)
   * scala> x.bimap(_.headOption, _.toLong + 1)
   * res0: (Option[String], Long) = (Some(foo),4)
   * }}}
   */
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  def rightFunctor[X]: Functor[F[X, ?]] =
    new RightFunctor[F, X] { val F = self }

  def leftFunctor[X]: Functor[F[?, X]] =
    new LeftFunctor[F, X] { val F = self }

  // derived methods
  /**
   * apply a function to the "left" functor
   */
  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, identity)

  /** The composition of two Bifunctors is itself a Bifunctor */
  def compose[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifunctor[F, G] {
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

private[cats] trait ComposedBifunctor[F[_, _], G[_, _]] extends Bifunctor[λ[(A, B) => F[G[A, B], G[A, B]]]] {
  def F: Bifunctor[F]
  def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] = {
    val innerBimap: G[A, B] => G[C, D] = gab => G.bimap(gab)(f, g)
    F.bimap(fab)(innerBimap, innerBimap)
  }
}

abstract private class LeftFunctor[F[_, _], X] extends Functor[F[?, X]] {
  implicit val F: Bifunctor[F]

  override def map[A, C](fax: F[A, X])(f: A => C): F[C, X] =
    F.bimap(fax)(f, identity)
}

abstract private class RightFunctor[F[_, _], X] extends Functor[F[X, ?]] {
  implicit val F: Bifunctor[F]

  override def map[A, C](fxa: F[X, A])(f: A => C): F[X, C] =
    F.bimap(fxa)(identity, f)
}
