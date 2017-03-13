package cats
package syntax

private[syntax] trait FunctorFilterSyntax1 {
  implicit def catsSyntaxUFunctorFilter[FA](fa: FA)(implicit U: Unapply[FunctorFilter, FA]): FunctorFilter.Ops[U.M, U.A] =
    new FunctorFilter.Ops[U.M, U.A]{
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait FunctorFilterSyntax extends FunctorFilter.ToFunctorFilterOps with FunctorFilterSyntax1
