package cats
package syntax

trait UnfoldableSyntax1 {
  implicit def catsSyntaxUUnfoldable[FA](fa: FA)(implicit U: Unapply[Unfoldable,FA]): Unfoldable.Ops[U.M, U.A] =
    new Unfoldable.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
      }
}

trait UnfoldableSyntax extends Unfoldable.ToUnfoldableOps with UnfoldableSyntax1
