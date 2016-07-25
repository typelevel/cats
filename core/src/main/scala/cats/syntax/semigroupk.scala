package cats
package syntax

private[syntax] trait SemigroupKSyntax1 {
  // TODO: use simulacrum instances eventually
  implicit def catsSyntaxUSemigroup[FA](fa: FA)(implicit U: Unapply[SemigroupK, FA]): SemigroupK.Ops[U.M, U.A] =
    new SemigroupK.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait SemigroupKSyntax extends SemigroupK.ToSemigroupKOps with SemigroupKSyntax1
