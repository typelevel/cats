package cats
package laws

import cats.syntax.all._

/**
 * Laws that must be obeyed by any rigid `Selective`.
 */
trait RigidSelectiveLaws[F[_]] extends SelectiveLaws[F] {
  implicit override def F: Selective[F]

  def selectiveApply[A, B](fa: F[A], ff: F[A => B]): IsEq[F[B]] =
    ff.apS(fa) <-> ff.ap(fa)

  def selectiveSkipOnRight[A, B](fb: F[B]): IsEq[F[B]] = {
    def ope = F.pure(sys.error("ope"): A => B)
    fb.map(b => Right(b)).select(ope) <-> fb
  }
}

object RigidSelectiveLaws {
  def apply[F[_]](implicit ev: Selective[F]): RigidSelectiveLaws[F] =
    new RigidSelectiveLaws[F] { def F: Selective[F] = ev }
}
