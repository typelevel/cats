package cats

import cats.functor.{Bifunctor, ComposedBifunctor}

/**
 *  A type class abstracting over types that give rise to two independent [[cats.Traverse]]s.
 */
trait Bitraverse[F[_, _]] extends Bifoldable[F] with Bifunctor[F] { self =>

  /** Traverse each side of the structure with the given functions */
  def bitraverse[G[_]: Applicative, A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D]): G[F[C, D]]

  /**
   * Sequence each side of the structure with the given functions.
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.implicits._
   *
   * scala> val rightSome: Option[String] Xor Option[Int] = Xor.right(Some(3))
   * scala> rightSome.bisequence
   * res0: Option[String Xor Int] = Some(Right(3))
   *
   * scala> val rightNone: Option[String] Xor Option[Int] = Xor.right(None)
   * scala> rightNone.bisequence
   * res1: Option[String Xor Int] = None
   *
   * scala> val leftSome: Option[String] Xor Option[Int] = Xor.left(Some("foo"))
   * scala> leftSome.bisequence
   * res2: Option[String Xor Int] = Some(Left(foo))
   *
   * scala> val leftNone: Option[String] Xor Option[Int] = Xor.left(None)
   * scala> leftNone.bisequence
   * res3: Option[String Xor Int] = None
   * }}}
   */
  def bisequence[G[_]: Applicative, A, B](fab: F[G[A], G[B]]): G[F[A, B]] =
    bitraverse(fab)(identity, identity)

  /** If F and G are both [[cats.Bitraverse]] then so is their composition F[G[_, _], G[_, _]] */
  def compose[G[_, _]](implicit ev: Bitraverse[G]): Bitraverse[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBitraverse[F, G] {
      val F = self
      val G = ev
    }

  override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
    bitraverse[Id, A, B, C, D](fab)(f, g)
}

object Bitraverse {
  def apply[F[_, _]](implicit F: Bitraverse[F]): Bitraverse[F] = F
}

private[cats] trait ComposedBitraverse[F[_, _], G[_, _]]
    extends Bitraverse[λ[(α, β) => F[G[α, β], G[α, β]]]]
    with    ComposedBifoldable[F, G]
    with    ComposedBifunctor[F, G] {
  def F: Bitraverse[F]
  def G: Bitraverse[G]

  override def bitraverse[H[_]: Applicative, A, B, C, D](
    fab: F[G[A, B], G[A, B]])(
    f: A => H[C], g: B => H[D]
  ): H[F[G[C, D], G[C, D]]] =
    F.bitraverse(fab)(
      gab => G.bitraverse(gab)(f, g),
      gab => G.bitraverse(gab)(f, g)
    )
}
