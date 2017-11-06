package cats.laws

import cats.CommutativeApplicative

trait CommutativeApplicativeLaws[F[_]] extends CommutativeApplyLaws[F] with ApplicativeLaws[F] {
  implicit override def F: CommutativeApplicative[F]
}

object CommutativeApplicativeLaws {
  def apply[F[_]](implicit ev: CommutativeApplicative[F]): CommutativeApplicativeLaws[F] =
    new CommutativeApplicativeLaws[F] { def F: CommutativeApplicative[F] = ev }
}
