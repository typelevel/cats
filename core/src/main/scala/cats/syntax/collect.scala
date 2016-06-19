package cats
package syntax

trait CollectSyntax extends Collect.ToCollectOps with CollectSyntax1

private[syntax] trait CollectSyntax1 {
  implicit def catsSyntaxUCollect[FA](fa: FA)(implicit U: Unapply[Collect,FA]): Collect.Ops[U.M, U.A] =
    new Collect.Ops[U.M, U.A]{
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}
