package cats
package syntax

trait FunctorSyntax1 {
  implicit def functorSyntaxU[FA](fa: FA)(implicit U: Unapply[Functor,FA]): Functor.Ops[U.M, U.A] =
    new Functor.Ops[U.M, U.A]{
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait FunctorSyntax extends Functor.ToFunctorOps with FunctorSyntax1
