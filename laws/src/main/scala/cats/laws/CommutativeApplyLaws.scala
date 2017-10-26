package cats.laws

import cats.CommutativeApply

/**
  * Laws that must be obeyed by any `CommutativeApply`.
  */
trait CommutativeApplyLaws[F[_]] extends ApplyLaws[F] {
  implicit override def F: CommutativeApply[F]

  def applyCommutative[A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): IsEq[F[C]] =
    F.map2(fa, fb)(f) <-> F.map2(fb, fa)((b, a) => f(a, b))

}

object CommutativeApplyLaws {
  def apply[F[_]](implicit ev: CommutativeApply[F]): CommutativeApplyLaws[F] =
    new CommutativeApplyLaws[F] { def F: CommutativeApply[F] = ev }
}
