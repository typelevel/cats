package cats
package syntax

trait BitraverseSyntax extends BitraverseSyntax1 {
  implicit final def catsSyntaxBitraverse[F[_, _]: Bitraverse, A, B](fab: F[A, B]): BitraverseOps[F, A, B] =
    new BitraverseOps[F, A, B](fab)
}

private[syntax] trait BitraverseSyntax1 {
  implicit final def catsSyntaxNestedBitraverse[F[_, _]: Bitraverse, G[_], A, B](
    fgagb: F[G[A], G[B]]
  ): NestedBitraverseOps[F, G, A, B] =
    new NestedBitraverseOps[F, G, A, B](fgagb)
}

final class BitraverseOps[F[_, _], A, B](private val fab: F[A, B]) extends AnyVal {
  def bitraverse[G[_]: Applicative, C, D](f: A => G[C], g: B => G[D])(implicit F: Bitraverse[F]): G[F[C, D]] =
    F.bitraverse(fab)(f, g)
}

final class NestedBitraverseOps[F[_, _], G[_], A, B](private val fgagb: F[G[A], G[B]]) extends AnyVal {
  def bisequence(implicit F: Bitraverse[F], G: Applicative[G]): G[F[A, B]] =
    F.bisequence(fgagb)
}

trait BitraverseSyntaxBinCompat0 {
  implicit final def catsSyntaxBitraverseBinCompat0[F[_, _]: Bitraverse, A, B](
    fab: F[A, B]
  ): BitraverseOpsBinCompat0[F, A, B] =
    new BitraverseOpsBinCompat0[F, A, B](fab)
  implicit final def catsSyntaxLeftNestedBitraverse[F[_, _]: Bitraverse, G[_], A, B](
    fgab: F[G[A], B]
  ): LeftNestedBitraverseOps[F, G, A, B] =
    new LeftNestedBitraverseOps[F, G, A, B](fgab)
}

final class BitraverseOpsBinCompat0[F[_, _], A, B](val fab: F[A, B]) extends AnyVal {

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
   *  scala> intAndString.leftTraverse(i => Option(i).filter(_ > 5))
   *  res1: Option[(Int, String)] = Some((7,test))
   *
   *  scala> intAndString.leftTraverse(i => Option(i).filter(_ < 5))
   *  res2: Option[(Int, String)] = None
   *  }}}
   */
  def leftTraverse[G[_], C](f: A => G[C])(implicit F: Bitraverse[F], G: Applicative[G]): G[F[C, B]] =
    F.bitraverse(fab)(f, G.pure(_))
}

final class LeftNestedBitraverseOps[F[_, _], G[_], A, B](val fgab: F[G[A], B]) extends AnyVal {

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
  def leftSequence(implicit F: Bitraverse[F], G: Applicative[G]): G[F[A, B]] =
    F.bitraverse(fgab)(identity, G.pure(_))
}
