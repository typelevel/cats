package cats
package laws

import cats.syntax.all._

/**
 * Laws that must be obeyed by any rigid `Selective`.
 */
trait RigidSelectiveLaws[F[_]] extends SelectiveLaws[F] {
  implicit override def F: Selective[F]

  def selectiveApply[A, B](fa: F[A], ff: F[A => B]): IsEq[F[B]] =
    ff.ap(fa) <-> {
      val left: F[Either[A => B, B]] = ff.map(Left(_))
      val right: F[(A => B) => B] = fa.map((a: A) => _(a))
      left.select(right)
    }

  private def ope[A] = F.pure(sys.error("ope!"): A)

  def selectiveSelectSkip[A, B](fb: F[B]): IsEq[F[B]] =
    fb.map(b => Right(b)).select(ope[A => B]) <-> fb

  def selectiveBranchSkipRight[A, B, C](fa: F[A], fl: F[A => C]): IsEq[F[C]] =
    fa.map(Left(_)).branch(fl)(ope[B => C]) <-> fa.map(Left(_)).select(fl)

  def selectiveBranchSkipLeft[A, B, C](fb: F[B], fr: F[B => C]): IsEq[F[C]] =
    fb.map(Right(_)).branch(ope[A => C])(fr) <-> fb.map(Left(_)).select(fr)
}

object RigidSelectiveLaws {
  def apply[F[_]](implicit ev: RigidSelective[F]): RigidSelectiveLaws[F] =
    new RigidSelectiveLaws[F] { def F: RigidSelective[F] = ev }
}
