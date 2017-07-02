package cats
package syntax

trait CartesianSyntax {
  implicit final def catsSyntaxCartesian[F[_], A](fa: F[A])(implicit F: Cartesian[F]): CartesianOps[F, A] =
    new CartesianOps[F, A] {
      val self = fa
      val typeClassInstance = F
    }
}

abstract class CartesianOps[F[_], A] extends Cartesian.Ops[F, A] {

  @deprecated("Replaced by an apply syntax, e.g. instead of (a |@| b).map(...) use (a, b).mapN(...)", "1.0.0-MF")
  final def |@|[B](fb: F[B]): CartesianBuilder[F]#CartesianBuilder2[A, B] =
    new CartesianBuilder[F] |@| self |@| fb

  final def *>[B](fb: F[B])(implicit F: Functor[F]): F[B] =
    F.map(typeClassInstance.product(self, fb)) { case (_, b) => b }

  final def <*[B](fb: F[B])(implicit F: Functor[F]): F[A] =
    F.map(typeClassInstance.product(self, fb)) { case (a, _) => a }

}
