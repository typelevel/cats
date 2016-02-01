package cats
package syntax

trait MonadSyntax1 {
  implicit def monadSyntaxU[FA](fa: FA)(implicit U: Unapply[Monad, FA]): Monad.Ops[U.M, U.A] =
    new Monad.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait MonadSyntax extends MonadSyntax1 {
  implicit def monadSyntax[F[_], A](fa: F[A])(implicit F: Monad[F]): Monad.Ops[F, A] =
    new Monad.Ops[F,A] {
      val self = fa
      val typeClassInstance = F
    }
}
