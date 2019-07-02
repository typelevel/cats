package cats
package laws

trait DecideableLaws[F[_]] extends ContravariantMonoidalLaws[F] {
  implicit override def F: Decideable[F]

  def decideableDecideRightAbsorption[A](fa: F[A]): IsEq[F[A]] =
    F.decide(F.trivial[A], fa)(Right.apply[A, A]) <-> fa

  def decideableSumAssociativity[A, B, C](
    fa: F[A],
    fb: F[B],
    fc: F[C]
  ): (F[Either[A, Either[B, C]]], F[Either[Either[A, B], C]]) =
    (F.sum(fa, F.sum(fb, fc)), F.sum(F.sum(fa, fb), fc))

  def decideableRightDistributivity[A, B, C](fa: F[A], fb: F[B], f: C => A, g: C => B): IsEq[F[Either[C, C]]] =
    F.contramap(F.sum(fa, fb))(
      (eit: Either[C, C]) =>
        eit.fold(
          f.andThen(Left.apply),
          g.andThen(Right.apply)
        )
    ) <-> F.sum(F.contramap(fa)(f), F.contramap(fb)(g))
}

object DecideableLaws {
  def apply[F[_]](implicit ev: Decideable[F]): DecideableLaws[F] =
    new DecideableLaws[F] { def F: Decideable[F] = ev }
}
