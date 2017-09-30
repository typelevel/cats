package cats

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

  def traverseUnordered[F[_]: CommutativeApplicative, A, B](sa: Set[A])(f: A => F[B]): F[Set[B]] =
    sa.foldLeft(Applicative[F].pure(Set.empty[B])) { (buf, a) =>
      Applicative[F].map2(buf, f(a))(_ + _)
    }

  def sequenceUnordered[F[_]: CommutativeApplicative, A](sa: Set[F[A]]): F[Set[A]] =
    sa.foldLeft(Applicative[F].pure(Set.empty[A])) { (acc, a) =>
      Applicative[F].map2(acc, a)(_ + _)
    }

}
