package cats
package syntax

private[syntax] trait TraverseSyntax1 {
  implicit def catsSyntaxUTraverse[FA](fa: FA)(implicit U: Unapply[Traverse, FA]): Traverse.Ops[U.M, U.A] =
    new Traverse.Ops[U.M, U.A]{
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait TraverseSyntax extends Traverse.ToTraverseOps with TraverseSyntax1
