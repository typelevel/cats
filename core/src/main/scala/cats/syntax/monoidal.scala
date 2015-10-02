package cats
package syntax

trait MonoidalSyntax1 {
  implicit def monoidalSyntaxU[FA](fa: FA)(implicit U: Unapply[Monoidal, FA]): MonoidalOps[U.M, U.A] =
    new MonoidalOps[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait MonoidalSyntax extends MonoidalSyntax1 {
  implicit def monoidalSyntax[F[_], A](fa: F[A])(implicit F: Monoidal[F]): MonoidalOps[F, A] =
    new MonoidalOps[F, A] {
      val self = fa
      val typeClassInstance = F
    }
}

abstract class MonoidalOps[F[_], A] extends Monoidal.Ops[F, A] {
  def |@|[B](fb: F[B]): MonoidalBuilder[F]#MonoidalBuilder2[A, B] =
    new MonoidalBuilder[F] |@| self |@| fb

  def *>[B](fb: F[B])(implicit F: Functor[F]): F[B] = F.map(typeClassInstance.product(self, fb)) { case (a, b) => b }

  def <*[B](fb: F[B])(implicit F: Functor[F]): F[A] = F.map(typeClassInstance.product(self, fb)) { case (a, b) => a }

}
