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
    sa.foldLeft(Applicative[F].pure(Set.empty[B])) { (acc, a) =>
      Applicative[F].map2(acc, f(a))(_ + _)
    }

  def sequenceUnordered[F[_]: CommutativeApplicative, A](sa: Set[F[A]]): F[Set[A]] =
    sa.foldLeft(Applicative[F].pure(Set.empty[A])) { (acc, a) =>
      Applicative[F].map2(acc, a)(_ + _)
    }


  def traverseUnorderedMap[F[_]: CommutativeApplicative, K, L, A, B](sa: Map[K, A])(f: (K, A) => F[(L, B)]): F[Map[L, B]] =
    sa.foldLeft(Applicative[F].pure(Map.empty[L, B])) { (acc, a) =>
      Applicative[F].map2(acc, f.tupled(a))(_ + _)
    }

  def sequenceUnorderedMap[F[_]: CommutativeApplicative, K, L, A](sa: Map[K, F[(K, A)]]): F[Map[K, A]] = {
    sa.foldLeft(Applicative[F].pure(Map.empty[K, A])) { (acc, a) =>
      Applicative[F].map2(acc, a._2)(_ + _)
    }
  }

}
