package cats
package syntax

private[syntax] trait CartesianSyntax1 {
  implicit def catsSyntaxUCartesian[FA](fa: FA)(implicit U: Unapply[Cartesian, FA]): CartesianOps[U.M, U.A] =
    new CartesianOps[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait CartesianSyntax extends CartesianSyntax1 {
  implicit def catsSyntaxCartesian[F[_], A](fa: F[A])(implicit F: Cartesian[F]): CartesianOps[F, A] =
    new CartesianOps[F, A] {
      val self = fa
      val typeClassInstance = F
    }
}

abstract class CartesianOps[F[_], A] extends Cartesian.Ops[F, A] {
  def |@|[B](fb: F[B]): CartesianBuilder[F]#CartesianBuilder2[A, B] =
    new CartesianBuilder[F] |@| self |@| fb

  def *>[B](fb: F[B])(implicit F: Functor[F]): F[B] = F.map(typeClassInstance.product(self, fb)) { case (a, b) => b }

  def <*[B](fb: F[B])(implicit F: Functor[F]): F[A] = F.map(typeClassInstance.product(self, fb)) { case (a, b) => a }

}
