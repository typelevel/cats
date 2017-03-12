package cats
package syntax

private[syntax] trait ComonadSyntax1 {
  implicit def catsSyntaxUComonad[FA](fa: FA)(implicit U: Unapply[Comonad, FA]): Comonad.Ops[U.M, U.A] =
    new Comonad.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait ComonadSyntax extends Comonad.ToComonadOps with ComonadSyntax1
