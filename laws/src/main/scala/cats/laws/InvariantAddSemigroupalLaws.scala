package cats
package laws

/**
 * Laws that are expected for any `cats.InvariantChoice`.
 */
trait InvariantChoiceLaws[F[_]] extends InvariantLaws[F] {
  implicit def I: InvariantChoice[F]
  implicit def F: Invariant[F] = I.invariant

  def choiceAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[Either[A, Either[B, C]]]] =
    I.choice(fa, I.choice(fb, fc)) <-> F.imap(I.choice(I.choice(fa, fb), fc))(rightToLeft)(leftToRight)

  private def leftToRight[A, B, C](l: Either[A, Either[B, C]]): Either[Either[A, B], C] =
    l match {
      case Left(a)         => Left(Left(a))
      case Right(Left(b))  => Left(Right(b))
      case Right(Right(c)) => Right(c)
    }

  private def rightToLeft[A, B, C](r: Either[Either[A, B], C]): Either[A, Either[B, C]] =
    r match {
      case Left(Left(a))  => Left(a)
      case Left(Right(b)) => Right(Left(b))
      case Right(c)       => Right(Right(c))
    }

}
object InvariantChoiceLaws {
  def apply[F[_]](implicit ev: InvariantChoice[F]): InvariantChoiceLaws[F] =
    new InvariantChoiceLaws[F] { def I: InvariantChoice[F] = ev }
}
