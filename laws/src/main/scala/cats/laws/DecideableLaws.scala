package cats
package laws

trait DecideableLaws[F[_]] extends ContravariantMonoidalLaws[F] {
  implicit override def F: Decideable[F]

  def decideableDecideRightAbsorption[A](fa: F[A]): IsEq[F[A]] =
    (F.decide(F.trivial[A], fa))(Right.apply[A, A]) <-> fa

  def decideableSumAssociativity[A, B, C](fa: F[A],
                                          fb: F[B],
                                          fc: F[C]): (F[Either[A, Either[B, C]]], F[Either[Either[A, B], C]]) =
    (F.sum(fa, F.sum(fb, fc)), F.sum(F.sum(fa, fb), fc))

  // What is the right distributivity law here?
}

object DecideableLaws {
  def apply[F[_]](implicit ev: Decideable[F]): DecideableLaws[F] =
    new DecideableLaws[F] { def F: Decideable[F] = ev }
}
