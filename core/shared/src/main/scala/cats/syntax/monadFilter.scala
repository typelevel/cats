package cats
package syntax

trait MonadFilterSyntax1 {
  implicit def monadFilterSyntaxU[FA](fa: FA)(implicit U: Unapply[MonadFilter,FA]): MonadFilter.Ops[U.M, U.A] =
    new MonadFilter.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait MonadFilterSyntax extends MonadFilter.ToMonadFilterOps with MonadFilterSyntax1
