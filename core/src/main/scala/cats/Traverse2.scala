package cats

import cats.functor.{Functor2, ComposedFunctor2}

import simulacrum.typeclass

/**
 *  A type class abstracting over types that give rise to two independent [[cats.Traverse]]s.
 */
@typeclass trait Traverse2[F[_, _]] extends Foldable2[F] with Functor2[F] { self =>

  /** Traverse each side of the structure with the given functions */
  def traverse2[G[_]: Applicative, A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D]): G[F[C, D]]

  /**
   * Sequence each side of the structure with the given functions.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val rightSome: Either[Option[String], Option[Int]] = Either.right(Some(3))
   * scala> rightSome.sequence2
   * res0: Option[Either[String, Int]] = Some(Right(3))
   *
   * scala> val rightNone: Either[Option[String], Option[Int]] = Either.right(None)
   * scala> rightNone.sequence2
   * res1: Option[Either[String, Int]] = None
   *
   * scala> val leftSome: Either[Option[String], Option[Int]] = Either.left(Some("foo"))
   * scala> leftSome.sequence2
   * res2: Option[Either[String, Int]] = Some(Left(foo))
   *
   * scala> val leftNone: Either[Option[String], Option[Int]] = Either.left(None)
   * scala> leftNone.sequence2
   * res3: Option[Either[String, Int]] = None
   * }}}
   */
  def sequence2[G[_]: Applicative, A, B](fab: F[G[A], G[B]]): G[F[A, B]] =
    traverse2(fab)(identity, identity)

  /** If F and G are both [[cats.Traverse2]] then so is their composition F[G[_, _], G[_, _]] */
  def compose[G[_, _]](implicit ev: Traverse2[G]): Traverse2[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedTraverse2[F, G] {
      val F = self
      val G = ev
    }

  override def map2[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
    traverse2[Id, A, B, C, D](fab)(f, g)
}

private[cats] trait ComposedTraverse2[F[_, _], G[_, _]]
    extends Traverse2[λ[(α, β) => F[G[α, β], G[α, β]]]]
    with    ComposedFoldable2[F, G]
    with    ComposedFunctor2[F, G] {
  def F: Traverse2[F]
  def G: Traverse2[G]

  override def traverse2[H[_]: Applicative, A, B, C, D](
    fab: F[G[A, B], G[A, B]])(
    f: A => H[C], g: B => H[D]
  ): H[F[G[C, D], G[C, D]]] =
    F.traverse2(fab)(
      gab => G.traverse2(gab)(f, g),
      gab => G.traverse2(gab)(f, g)
    )
}
