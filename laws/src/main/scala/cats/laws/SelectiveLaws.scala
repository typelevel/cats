package cats.laws

import cats.Selective

/**
 * Laws that must be obeyed by any `Selective`.
 */
trait SelectiveLaws[F[_]] {
  implicit def F: Selective[F]

  private def either[A, B, C](f: A => C, g: B => C)(x: Either[A, B]): C =
    x match {
      case Left(a) => f(a)
      case Right(b) => g(b)
    }

  def selectiveIdentity[A](x: F[Either[A, A]]): IsEq[F[A]] = {
    F.select(x)(F.pure(Predef.identity)) <-> F.map(x)(either(Predef.identity, Predef.identity))
  }

  def selectiveDistributivity[A, B](x: Either[A, B], y: F[A => B], z: F[A => B]): IsEq[F[B]] = {
    val lhs = F.select(F.pure(x))(F.applicative.*>(y)(z))
    val rhs = F.applicative.*>(F.select(F.pure(x))(y))(F.select(F.pure(x))(z))
    lhs <-> rhs
  }

  // TODO associativity law

  // TODO the law for when F is also a monad (must skip unnecessary effects)

}

object SelectiveLaws {
  def apply[F[_]](implicit ev: Selective[F]): SelectiveLaws[F] =
    new SelectiveLaws[F] { def F: Selective[F] = ev }
}
