package cats
package syntax

trait FunctorSyntax {
  implicit def functorSyntax[F[_]: Functor, A](fa: F[A]) =
    Functor.ops.toFunctorOps(fa)
}
