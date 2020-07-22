package cats
package laws

trait DecidableLaws[F[_]] extends ContravariantMonoidalLaws[F] {
  implicit override def F: Decidable[F]

  def decideableDecideRightAbsorption[A](fa: F[A]): IsEq[F[A]] =
    F.decide(F.trivial[A], fa)(Right.apply[A, A]) <-> fa

  def decideableSumAssociativity[A, B, C](
    fa: F[A],
    fb: F[B],
    fc: F[C]
  ): IsEq[F[Either[Either[A, B], C]]] =
    F.contramap[Either[A, Either[B, C]], Either[Either[A, B], C]](
      F.sum(fa, F.sum(fb, fc)))(
      {
        case Left(Left(x)) => Left(x)
        case Left(Right(x)) => Right(Left(x))
        case Right(x) => Right(Right(x))
      }
    ) <-> F.sum(F.sum(fa, fb), fc)

  def decideableRightDistributivity[A, B, C](fa: F[A], fb: F[B], f: C => A, g: C => B): IsEq[F[Either[C, C]]] =
    F.contramap(F.sum(fa, fb))((eit: Either[C, C]) =>
      eit.fold(
        f.andThen(Left.apply),
        g.andThen(Right.apply)
      )
    ) <-> F.sum(F.contramap(fa)(f), F.contramap(fb)(g))
}

object DecidableLaws {
  def apply[F[_]](implicit ev: Decidable[F]): DecidableLaws[F] =
    new DecidableLaws[F] { def F: Decidable[F] = ev }
}
