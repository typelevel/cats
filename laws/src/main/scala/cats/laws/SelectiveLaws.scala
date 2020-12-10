package cats
package laws

import cats.syntax.all._

/**
 * Laws that must be obeyed by any `Selective`.
 */
trait SelectiveLaws[F[_]] extends ApplicativeLaws[F] {
  implicit override def F: Selective[F]

  def selectiveIdentity[A, B](faa: F[Either[A, A]]): IsEq[F[A]] =
    faa.select(F.pure(identity)) <-> faa.map(_.merge)

  def selectiveDistributivity[A, B](ab: Either[A, B], ff1: F[A => B], ff2: F[A => B]): IsEq[F[B]] =
    F.pure(ab).select(ff1 *> ff2) <-> F.pure(ab).select(ff1) *> F.pure(ab).select(ff2)

  // TODO associativity

  def selectiveBranchConsistency[A, B, C](fab: F[Either[A, B]], fl: F[A => C], fr: F[B => C]): IsEq[F[C]] = {
    fab.branch(fl)(fr) <-> {
      val lhs = {
        val innerLhs: F[Either[A, Either[B, C]]] = F.map(fab)(_.map(Left(_)))
        val innerRhs: F[A => Either[B, C]] = F.map(fl)(_.andThen(Right(_)))
        F.select(innerLhs)(innerRhs)
      }
      F.select(lhs)(fr)
    }
  }

  def selectiveIfSConsistency[A](fb: F[Boolean], ft: F[A], ff: F[A]): IsEq[F[A]] = {
    fb.ifS(ft)(ff) <-> {
      val condition: F[Either[Unit, Unit]] = F.map(fb)(p => if (p) Left(()) else Right(()))
      val left: F[Unit => A] = F.map(ft)(Function.const)
      val right: F[Unit => A] = F.map(ff)(Function.const)
      F.branch(condition)(left)(right)
    }
  }

  def selectiveWhenSConsistency[A](fb: F[Boolean], fa: F[Unit]): IsEq[F[Unit]] = {
    fb.whenS(fa) <-> F.ifS(fb)(fa)(F.unit)
  }
}

object SelectiveLaws {
  def apply[F[_]](implicit ev: Selective[F]): SelectiveLaws[F] =
    new SelectiveLaws[F] { def F: Selective[F] = ev }
}
