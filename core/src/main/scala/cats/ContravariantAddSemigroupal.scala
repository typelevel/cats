package cats

import simulacrum.typeclass

/**
 * [[ContravariantAddSemigroupal]] is an Additive Semigroupal that is also contravariant.
 *
 * Must obey the laws defined in cats.laws.ContravariantAddMonoidalLaws.
 */
@typeclass trait ContravariantAddSemigroupal[F[_]] extends InvariantAddSemigroupal[F] {

  def contravariant: Contravariant[F]

  override def invariant: Invariant[F] = contravariant

  /**
   * `decide` combines two values in `F` and applies the function `f` to its result.
   * It can be seen as the additive version of `contramap2`.
   */
  def decide[A, B, C](fa: F[A], fb: F[B])(f: C => Either[A, B]): F[C] =
    contravariant.contramap(sum(fa, fb))(f)
}
