package cats

import simulacrum.typeclass

/**
 * [[ContravariantChoice]] is an Additive Semigroupal that is also contravariant.
 *
 * Must obey the laws defined in cats.laws.ContravariantChoosableLaws.
 */
@typeclass trait ContravariantChoice[F[_]] extends InvariantChoice[F] {

  def contravariant: Contravariant[F]

  override def invariant: Invariant[F] = contravariant

  /**
   * `decide` combines two values in `F` and applies the function `f` to its result.
   * It can be seen as the additive version of `contramap2`.
   */
  def decide[A, B, C](fa: F[A], fb: F[B])(f: C => Either[A, B]): F[C] =
    contravariant.contramap(choice(fa, fb))(f)
}

object ContravariantChoice {
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[ContravariantChoice]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: ContravariantChoice[F]): ContravariantChoice[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllContravariantChoiceOps[F[_], A](target: F[A])(implicit tc: ContravariantChoice[F]): AllOps[F, A] {
      type TypeClassType = ContravariantChoice[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = ContravariantChoice[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: ContravariantChoice[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def decide[B, C](fb: F[B])(f: C => Either[A, B]): F[C] = typeClassInstance.decide[A, B, C](self, fb)(f)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with InvariantChoice.AllOps[F, A] {
    type TypeClassType <: ContravariantChoice[F]
  }
  trait ToContravariantChoiceOps extends Serializable {
    implicit def toContravariantChoiceOps[F[_], A](target: F[A])(implicit tc: ContravariantChoice[F]): Ops[F, A] {
      type TypeClassType = ContravariantChoice[F]
    } =
      new Ops[F, A] {
        type TypeClassType = ContravariantChoice[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToContravariantChoiceOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
