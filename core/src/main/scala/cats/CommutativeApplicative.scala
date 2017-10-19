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
@typeclass trait CommutativeApplicative[F[_]] extends Applicative[F] with CommutativeApply[F] {
  def traverseUnordered[A, B](sa: Set[A])(f: A => F[B]): F[Set[B]] =
    sa.foldLeft(pure(Set.empty[B])) { (acc, a) =>
      map2(acc, f(a))(_ + _)
    }

  def sequenceUnordered[A](sa: Set[F[A]]): F[Set[A]] =
    sa.foldLeft(pure(Set.empty[A])) { (acc, a) =>
      map2(acc, a)(_ + _)
    }


  def traverseUnorderedMap[K, L, A, B](sa: Map[K, A])(f: (K, A) => F[(L, B)]): F[Map[L, B]] =
    sa.foldLeft(pure(Map.empty[L, B])) { (acc, a) =>
      map2(acc, f.tupled(a))(_ + _)
    }

  def sequenceUnorderedMap[K, L, A](sa: Map[K, F[(K, A)]]): F[Map[K, A]] = {
    sa.foldLeft(pure(Map.empty[K, A])) { (acc, a) =>
      map2(acc, a._2)(_ + _)
    }
  }
}

@typeclass trait TraverseUnordered[F[_]] {
  def traverseUnordered[G[_]: CommutativeApplicative, A, B](sa: F[A])(f: A => G[B]): G[F[B]]

  def sequenceUnordered[G[_]: CommutativeApplicative, A](fga: F[G[A]]): G[F[A]] =
    traverseUnordered(fga)(identity)
}

@typeclass trait NonEmptyTraverseUnordered[F[_]] {
  def nonEmptyTraverseUnordered[G[_]: CommutativeApply, A, B](sa: F[A])(f: A => G[B]): G[F[B]]

  def nonEmptySequenceUnordered[G[_]: CommutativeApply, A](fga: F[G[A]]): G[F[A]] =
    nonEmptyTraverseUnordered(fga)(identity)
}

@typeclass trait NonEmptyCommutativeParallel[F[_], M[_]] {
  def commutativeApply: CommutativeApply[F]
  def commutativeFlatMap: CommutativeFlatMap[M]

  def sequential: F ~> M
  def parallel: M ~> F
}

@typeclass trait CommutativeParallel[F[_], M[_]] extends NonEmptyCommutativeParallel[F, M] {
  def commutativeApplicative: CommutativeApplicative[F]
  def commutativeMonad: CommutativeMonad[M]

  def commutativeApply = commutativeApplicative
  def commutativeFlatMap = commutativeMonad
}
