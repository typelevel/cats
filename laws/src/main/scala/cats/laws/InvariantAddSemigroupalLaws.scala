package cats
package laws

/**
 * Laws that are expected for any `cats.InvariantAddSemigroupal`.
 */
trait InvariantAddSemigroupalLaws[F[_]] extends InvariantLaws[F] {
  implicit def I: InvariantAddSemigroupal[F]
  implicit def F: Invariant[F] = I.invariant

  def sumAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[Either[A, Either[B, C]]]] =
    I.sum(fa, I.sum(fb, fc)) <-> F.imap(I.sum(I.sum(fa, fb), fc))(rightToLeft)(leftToRight)

  private def leftToRight[A, B, C](l: Either[A, Either[B, C]]): Either[Either[A, B], C] = l match {
    case Left(a)         => Left(Left(a))
    case Right(Left(b))  => Left(Right(b))
    case Right(Right(c)) => Right(c)
  }

  private def rightToLeft[A, B, C](r: Either[Either[A, B], C]): Either[A, Either[B, C]] = r match {
    case Left(Left(a))  => Left(a)
    case Left(Right(b)) => Right(Left(b))
    case Right(c)       => Right(Right(c))
  }

}
object InvariantAddSemigroupalLaws {
  def apply[F[_]](implicit ev: InvariantAddSemigroupal[F]): InvariantAddSemigroupalLaws[F] =
    new InvariantAddSemigroupalLaws[F] { def I: InvariantAddSemigroupal[F] = ev }
}
