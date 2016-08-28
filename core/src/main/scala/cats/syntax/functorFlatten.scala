package cats
package syntax

private[syntax] trait FunctorFlattenSyntax1 {
  implicit def catsSyntaxUFunctorFlatten[FA](fa: FA)(implicit U: Unapply[FunctorFlatten, FA]): FunctorFlatten.Ops[U.M, U.A] =
    new FunctorFlatten.Ops[U.M, U.A]{
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait FunctorFlattenSyntax extends FunctorFlatten.ToFunctorFlattenOps with FunctorFlattenSyntax1
