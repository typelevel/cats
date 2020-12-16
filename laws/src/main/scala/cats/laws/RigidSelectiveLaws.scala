package cats
package laws

import cats.syntax.all._

/**
 * Laws that must be obeyed by any rigid `Selective`.
 */
trait RigidSelectiveLaws[F[_]] extends ApplicativeLaws[F] {
  implicit override def F: Selective[F]

  def selectiveApply[A, B](fa: F[A], ff: F[A => B]): IsEq[F[B]] =
    ff.apS(fa) <-> ff.ap(fa)
}

object RigidSelectiveLaws {
  def apply[F[_]](implicit ev: Selective[F]): RigidSelectiveLaws[F] =
    new RigidSelectiveLaws[F] { def F: Selective[F] = ev }
}
