package cats.laws

import cats.Functor
import cats.syntax.functor._

import cats.fix.Fix

trait FixLaws[F[_]] {
  implicit def F: Functor[F]


  def cataReflectionLaw(fix: Fix[F]): IsEq[Fix[F]] =
    fix.cata[Fix[F]](Fix(_)) <-> fix

  def cataCancellationLaw[A](unFix: F[Fix[F]], algebra: F[A] => A): IsEq[A] =
    Fix(unFix).cata[A](algebra) <-> algebra(unFix.map(_.cata[A](algebra)))

}

object FixLaws {
  def apply[F[_]](implicit ev: Functor[F]): FixLaws[F] =
    new FixLaws[F] {
      def F: Functor[F] = ev
    }
}

