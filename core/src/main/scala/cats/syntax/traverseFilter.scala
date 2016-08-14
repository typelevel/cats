package cats
package syntax

trait TraverseFilterSyntax extends TraverseFilter.ToTraverseFilterOps with TraverseFilterSyntax1

private[syntax] trait TraverseFilterSyntax1 {
  implicit def catsSyntaxUTraverseFilter[FA](fa: FA)(implicit U: Unapply[TraverseFilter, FA]): TraverseFilter.Ops[U.M, U.A] =
    new TraverseFilter.Ops[U.M, U.A]{
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}
