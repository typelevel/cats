package cats
package laws

import cats.syntax.all._

/**
 * Laws that must be obeyed by any `Selective`.
 */
trait SelectiveLaws[F[_]] extends ApplicativeLaws[F] {
  implicit override def F: Selective[F]

  def selectiveIdentity[A, B](faa: F[Either[A, A]]): IsEq[F[A]] =
    faa.select[A, A](F.pure(identity)) <-> faa.map(_.merge)

  def selectiveDistributivity[A, B](ab: Either[A, B], ff1: F[A => B], ff2: F[A => B]): IsEq[F[B]] =
    F.pure(ab).select(ff1 *> ff2) <-> F.pure(ab).select(ff1) *> F.pure(ab).select(ff2)

  def selectiveWhenSConsistency[A](fb: F[Boolean], fa: F[Unit]): IsEq[F[Unit]] = {
    fb.whenS(fa) <-> F.ifS(fb)(fa)(F.unit)
  }
}

object SelectiveLaws {
  def apply[F[_]](implicit ev: Selective[F]): SelectiveLaws[F] =
    new SelectiveLaws[F] { def F: Selective[F] = ev }
}
