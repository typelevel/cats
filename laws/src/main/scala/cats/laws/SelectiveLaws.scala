package cats
package laws

import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.selective._

/**
 * Laws that must be obeyed by any `Selective`.
 */
trait SelectiveLaws[F[_]] extends ApplicativeLaws[F] {
  implicit override def F: Selective[F]

  def selectiveIdentity[A, B](faa: F[Either[A, A]]): IsEq[F[A]] =
    faa.select[A, A](F.pure(identity)) <-> faa.map(_.merge)

  def selectiveDistributivity[A, B](ab: Either[A, B], ff1: F[A => B], ff2: F[A => B]): IsEq[F[B]] =
    F.pure(ab).select(ff1 *> ff2) <-> F.pure(ab).select(ff1) *> F.pure(ab).select(ff2)

  def selectAssociativity[A, B, C](fa: F[Either[A, B]], fb: F[Either[C, A => B]], fc: F[C => A => B]): IsEq[F[B]] = {
    val fa0 = fa.map(_.map(_.asRight[(C, A)]))
    val fb0 = fb.map { either => (a: A) => either.bimap(c => (c, a), f => f(a)) }
    val fc0 = fc.map(Function.uncurried(_).tupled)
    fa.select(fb.select(fc)) <-> fa0.select(fb0).select(fc0)
  }

  def branchSelectConsistency[A, B, C](fab: F[Either[A, B]], fl: F[A => C], fr: F[B => C]): IsEq[F[C]] = {
    fab.branch(fl)(fr) <-> {
      val innerLhs: F[Either[A, Either[B, C]]] = F.map(fab)(_.map(Left(_)))
      val innerRhs: F[A => Either[B, C]] = F.map(fl)(_.andThen(Right(_)))
      val lhs = F.select(innerLhs)(innerRhs)
      F.select(lhs)(fr)
    }
  }
}

object SelectiveLaws {
  def apply[F[_]](implicit ev: Selective[F]): SelectiveLaws[F] =
    new SelectiveLaws[F] { def F: Selective[F] = ev }
}
