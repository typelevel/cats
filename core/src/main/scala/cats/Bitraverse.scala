package cats

import simulacrum.{noop, typeclass}

/**
 *  A type class abstracting over types that give rise to two independent [[cats.Traverse]]s.
 */
@typeclass trait Bitraverse[F[_, _]] extends Bifoldable[F] with Bifunctor[F] { self =>

  /**
   * Traverse each side of the structure with the given functions.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
   *
   * scala> ("1", "2").bitraverse(parseInt, parseInt)
   * res0: Option[(Int, Int)] = Some((1,2))
   *
   * scala> ("1", "two").bitraverse(parseInt, parseInt)
   * res1: Option[(Int, Int)] = None
   * }}}
   */
  def bitraverse[G[_]: Applicative, A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D]): G[F[C, D]]

  /**
   * Invert the structure from F[G[A], G[B]] to G[F[A, B]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val rightSome: Either[Option[String], Option[Int]] = Either.right(Some(3))
   * scala> rightSome.bisequence
   * res0: Option[Either[String, Int]] = Some(Right(3))
   *
   * scala> val rightNone: Either[Option[String], Option[Int]] = Either.right(None)
   * scala> rightNone.bisequence
   * res1: Option[Either[String, Int]] = None
   *
   * scala> val leftSome: Either[Option[String], Option[Int]] = Either.left(Some("foo"))
   * scala> leftSome.bisequence
   * res2: Option[Either[String, Int]] = Some(Left(foo))
   *
   * scala> val leftNone: Either[Option[String], Option[Int]] = Either.left(None)
   * scala> leftNone.bisequence
   * res3: Option[Either[String, Int]] = None
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

  /**
   *  Traverse over the left side of the structure.
   *  For the right side, use the standard `traverse` from [[cats.Traverse]].
   *
   *  Example:
   *  {{{
   *  scala> import cats.implicits._
   *
   *  scala> val intAndString: (Int, String) = (7, "test")
   *
   *  scala> Bitraverse[Tuple2].leftTraverse(intAndString)(i => Option(i).filter(_ > 5))
   *  res1: Option[(Int, String)] = Some((7,test))
   *
   *  scala> Bitraverse[Tuple2].leftTraverse(intAndString)(i => Option(i).filter(_ < 5))
   *  res2: Option[(Int, String)] = None
   *  }}}
   */
  @noop
  def leftTraverse[G[_], A, B, C](fab: F[A, B])(f: A => G[C])(implicit G: Applicative[G]): G[F[C, B]] =
    bitraverse(fab)(f, G.pure(_))

  /**
   * Sequence the left side of the structure.
   * For the right side, use the standard `sequence` from [[cats.Traverse]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val optionalErrorRight: Either[Option[String], Int] = Either.right(123)
   * scala> optionalErrorRight.leftSequence
   * res1: Option[Either[String, Int]] = Some(Right(123))
   *
   * scala> val optionalErrorLeftSome: Either[Option[String], Int] = Either.left(Some("something went wrong"))
   * scala> optionalErrorLeftSome.leftSequence
   * res2: Option[Either[String, Int]] = Some(Left(something went wrong))
   *
   * scala> val optionalErrorLeftNone: Either[Option[String], Int] = Either.left(None)
   * scala> optionalErrorLeftNone.leftSequence
   * res3: Option[Either[String,Int]] = None
   * }}}
   */
  @noop
  def leftSequence[G[_], A, B](fgab: F[G[A], B])(implicit G: Applicative[G]): G[F[A, B]] =
    bitraverse(fgab)(identity, G.pure(_))
}

private[cats] trait ComposedBitraverse[F[_, _], G[_, _]]
    extends Bitraverse[λ[(α, β) => F[G[α, β], G[α, β]]]]
    with ComposedBifoldable[F, G]
    with ComposedBifunctor[F, G] {
  def F: Bitraverse[F]
  def G: Bitraverse[G]

  override def bitraverse[H[_]: Applicative, A, B, C, D](fab: F[G[A, B], G[A, B]])(
    f: A => H[C],
    g: B => H[D]
  ): H[F[G[C, D], G[C, D]]] =
    F.bitraverse(fab)(
      gab => G.bitraverse(gab)(f, g),
      gab => G.bitraverse(gab)(f, g)
    )
}
