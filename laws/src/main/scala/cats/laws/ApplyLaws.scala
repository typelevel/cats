package cats
package laws

import cats.syntax.apply._
import cats.syntax.functor._

/**
 * Laws that must be obeyed by any `Apply`.
 */
trait ApplyLaws[F[_]] extends FunctorLaws[F] with SemigroupalLaws[F] {
  implicit override def F: Apply[F]

  def applyComposition[A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C]): IsEq[F[C]] = {
    val compose: (B => C) => (A => B) => (A => C) = _.compose
    fbc.ap(fab.ap(fa)) <-> fbc.map(compose).ap(fab).ap(fa)
  }

  def map2ProductConsistency[A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): IsEq[F[C]] =
    F.map(F.product(fa, fb)) { case (a, b) => f(a, b) } <-> F.map2(fa, fb)(f)

  def map2EvalConsistency[A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): IsEq[F[C]] =
    F.map2(fa, fb)(f) <-> F.map2Eval(fa, Eval.now(fb))(f).value

  def productRConsistency[A, B](fa: F[A], fb: F[B]): IsEq[F[B]] =
    F.productR(fa)(fb) <-> F.map2(fa, fb)((_, b) => b)

  def productLConsistency[A, B](fa: F[A], fb: F[B]): IsEq[F[A]] =
    F.productL(fa)(fb) <-> F.map2(fa, fb)((a, _) => a)

  def selectAConsistency[A, B](fab: F[Either[A, B]], ff: F[A => B]): IsEq[F[B]] =
    F.selectA(fab)(ff) <-> F.map2(fab, ff) {
      case (Left(a), f)  => f(a)
      case (Right(b), _) => b
    }
}

object ApplyLaws {
  def apply[F[_]](implicit ev: Apply[F]): ApplyLaws[F] =
    new ApplyLaws[F] { def F: Apply[F] = ev }
}
