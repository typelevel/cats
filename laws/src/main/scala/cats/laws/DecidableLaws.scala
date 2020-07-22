package cats
package laws

trait DecidableLaws[F[_]] extends ContravariantMonoidalLaws[F] {
  implicit override def F: Decidable[F]

  def decidableDecideLeftIdentity[A](fa: F[A]): IsEq[F[A]] =
    F.decide(fa, F.trivial[A])(Left.apply[A, A]) <-> fa

  def decidableDecideRightIdentity[A](fa: F[A]): IsEq[F[A]] =
    F.decide(F.trivial[A], fa)(Right.apply[A, A]) <-> fa

  def decidableSumAssociativity[A, B, C](
    fa: F[A],
    fb: F[B],
    fc: F[C]
  ): IsEq[F[Either[Either[A, B], C]]] =
    F.contramap[Either[A, Either[B, C]], Either[Either[A, B], C]](F.sum(fa, F.sum(fb, fc)))(
      {
        case Left(Left(x))  => Left(x)
        case Left(Right(x)) => Right(Left(x))
        case Right(x)       => Right(Right(x))
      }
    ) <-> F.sum(F.sum(fa, fb), fc)

  def decidableRightDistributivity[A, B, C](fa: F[A], fb: F[B], f: C => A, g: C => B): IsEq[F[Either[C, C]]] =
    F.contramap(F.sum(fa, fb))((eit: Either[C, C]) =>
      eit.fold(
        f.andThen(Left.apply),
        g.andThen(Right.apply)
      )
    ) <-> F.sum(F.contramap(fa)(f), F.contramap(fb)(g))

  def decidableRightDistributivitySum[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[(A, Either[B, C])]] =
    F.product(fa, F.sum(fb, fc)) <->
      F.contramap(F.sum(F.product(fa, fb), F.product(fa, fc)))({
        case (a, Left(b))  => Left((a, b))
        case (a, Right(c)) => Right((a, c))
      })
}

object DecidableLaws {
  def apply[F[_]](implicit ev: Decidable[F]): DecidableLaws[F] =
    new DecidableLaws[F] { def F: Decidable[F] = ev }
}
