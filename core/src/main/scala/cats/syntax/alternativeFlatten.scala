package cats
package syntax

private[syntax] trait AlternativeFlattenSyntax1 {
  implicit def catsSyntaxUAlternativeFlatten[FA](fa: FA)(implicit U: Unapply[AlternativeFlatten, FA]): AlternativeFlatten.Ops[U.M, U.A] =
    new AlternativeFlatten.Ops[U.M, U.A]{
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait AlternativeFlattenSyntax extends AlternativeFlatten.ToAlternativeFlattenOps with AlternativeFlattenSyntax1
