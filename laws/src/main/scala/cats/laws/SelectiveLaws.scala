package cats
package laws

import cats.syntax.functor._

/**
 * Laws that must be obeyed by any `Selective`.
 */
trait SelectiveLaws[F[_]] extends ApplicativeLaws[F] {
  implicit override def F: Selective[F]

  def selectiveIdentity[A, B](faa: F[Either[A, A]]): IsEq[F[A]] =
    F.select(faa)(F.pure(identity)) <-> faa.map(_.merge)

  def selectiveDistributivity(ab: Either[A, B], ff1: F[A => B], ff2: F[A => B]): IsEq[F[A]] =
    F.select(F.pure(fab))(ff1 *> ff2) <-> F.select(pure(fab))(fa1) *> F.select(pure(fab))(fa2)

  // TODO associativity
}

object SelectiveLaws {
  def apply[F[_]](implicit ev: Selective[F]): SelectiveLaws[F] =
    new SelectiveLaws[F] { def F: Selective[F] = ev }
}
