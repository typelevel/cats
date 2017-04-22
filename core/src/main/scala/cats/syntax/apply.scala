package cats
package syntax

trait ApplySyntax {
  implicit final def catsSyntaxApply[F[_], A](fa: F[A])(implicit F: Apply[F]): Apply.Ops[F, A] =
    new Apply.Ops[F, A] {
      val self = fa
      val typeClassInstance = F
    }
}
