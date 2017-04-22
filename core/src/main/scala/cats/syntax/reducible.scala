package cats
package syntax

private[syntax] trait ReducibleSyntax1 {
  implicit def catsSyntaxUReducible[FA](fa: FA)(implicit U: Unapply[Reducible, FA]): Reducible.Ops[U.M, U.A] =
    new Reducible.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait ReducibleSyntax extends Reducible.ToReducibleOps with ReducibleSyntax1
