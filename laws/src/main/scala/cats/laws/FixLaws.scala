package cats.laws

import cats.Functor
import cats.syntax.functor._

import cats.fix.Fix

trait FixLaws[F[_]] {
  implicit def F: Functor[F]


  def foldReflectionLaw(fix: Fix[F]): IsEq[Fix[F]] =
    fix.fold[Fix[F]](Fix(_)) <-> fix

  def foldCancellationLaw[A](unFix: F[Fix[F]], algebra: F[A] => A) =
    Fix(unFix).fold[A](algebra) <-> algebra(unFix.map(_.fold[A](algebra)))

}

object FixLaws {
  def apply[F[_]](implicit ev: Functor[F]): FixLaws[F] =
    new FixLaws[F] { def F: Functor[F] = ev }
}

