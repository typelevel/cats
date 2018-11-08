package cats
package laws

trait DecideableLaws[F[_]] extends ContravariantMonoidalLaws[F] {
  implicit override def F: Decideable[F]

  def decideableDecideRightAbsorption[A](fa: F[A]): IsEq[F[A]] =
    (F.decide(fa, F.trivial[A])(Right.apply[A, A])) <-> F.trivial[A]

  def decideableDecideLeftUnit[A](fa: F[A]): IsEq[F[A]] =
    (F.decide(F.trivial[A], fa))(Right.apply[A, A]) <-> fa

  def decideableLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: B => A): IsEq[F[B]] = F.decide(fa, fa2)(f andThen Left.apply) <-> F.contramap(fa)(f)

  def decideableRightDistributivity[A, B](fa: F[A], fa2: F[A], f: B => A): IsEq[F[B]] = F.decide(fa, fa2)(f andThen Right.apply) <-> F.contramap(fa2)(f)

}

object DecideableLaws {
  def apply[F[_]](implicit ev: Decideable[F]): DecideableLaws[F] =
    new DecideableLaws[F] { def F: Decideable[F] = ev }
}
