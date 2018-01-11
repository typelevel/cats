package cats

import simulacrum.typeclass

/**
  * `UnorderedTraverse` is like a `Traverse` for unordered containers.
  */
@typeclass trait UnorderedTraverse[F[_]] extends UnorderedFoldable[F] {
  def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: F[A])(f: A => G[B]): G[F[B]]

  def unorderedSequence[G[_]: CommutativeApplicative, A](fga: F[G[A]]): G[F[A]] =
    unorderedTraverse(fga)(identity)
}
