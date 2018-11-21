package cats
package laws

import cats.data.INothing
import cats.syntax.all._

trait InvariantSemiringalLaws[F[_]] extends InvariantMonoidalLaws[F] with InvariantAddMonoidalLaws[F] {
  implicit override def I: InvariantSemiringal[F]
  override def F: InvariantSemiringal[F] = I

  def semiringalRightAbsorption[A](fa: F[A]): IsEq[F[INothing]] =
    I.product[A, INothing](fa, I.zero).imap(_._2)(INothing.absurd) <-> I.zero

  def semiringalRightDistributivity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): IsEq[F[(Either[A, B], C)]] =
    I.product(I.sum(fa, fb), fc) <-> I.sum(I.product(fa, fc), I.product(fb, fc)).imap(eitherToTuple)(tupleToEither)

  private def eitherToTuple[A, B, C](e: Either[(A, C), (B, C)]): (Either[A, B], C) = e match {
    case Left((a, c))  => (Left(a), c)
    case Right((b, c)) => (Right(b), c)
  }

  private def tupleToEither[A, B, C](t: (Either[A, B], C)): Either[(A, C), (B, C)] = t match {
    case (Left(a), c)  => Left((a, c))
    case (Right(b), c) => Right((b, c))
  }
}

object InvariantSemiringalLaws {
  def apply[F[_]](implicit ev: InvariantSemiringal[F]): InvariantSemiringalLaws[F] =
    new InvariantSemiringalLaws[F] { def I: InvariantSemiringal[F] = ev }
}
