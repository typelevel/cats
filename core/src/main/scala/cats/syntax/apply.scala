package cats
package syntax

private[syntax] trait ApplySyntax1 {
  implicit def catsSyntaxUApply[FA](fa: FA)(implicit U: Unapply[Apply, FA]): Apply.Ops[U.M, U.A] =
    new Apply.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait ApplySyntax extends ApplySyntax1 {
  implicit def catsSyntaxApply[F[_], A](fa: F[A])(implicit F: Apply[F]): Apply.Ops[F, A] =
    new Apply.Ops[F, A] {
      val self = fa
      val typeClassInstance = F
    }
}
