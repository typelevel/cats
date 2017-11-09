package cats

import cats.kernel.CommutativeMonoid
import simulacrum.typeclass

/**
  * Commutative Applicative.
  *
  * Further than an Applicative, which just allows composition of independent effectful functions,
  * in a Commutative Applicative those functions can be composed in any order, which guarantees
  * that their effects do not interfere.
  *
  * Must obey the laws defined in cats.laws.CommutativeApplicativeLaws.
  */
@typeclass trait CommutativeApplicative[F[_]] extends Applicative[F] with CommutativeApply[F]

object CommutativeApplicative {
  def monoid[F[_], A](implicit f: CommutativeApplicative[F], monoid: CommutativeMonoid[A]): CommutativeMonoid[F[A]] =
    new CommutativeApplicativeMonoid[F, A](f, monoid)
}

private[cats] class CommutativeApplicativeMonoid[F[_], A](f: CommutativeApplicative[F], monoid: CommutativeMonoid[A])
  extends ApplicativeMonoid[F, A](f, monoid) with CommutativeMonoid[F[A]]