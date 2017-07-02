package cats
package syntax

trait AlignSyntax {
  implicit final def catsSyntaxAlign[F[_], A](fa: F[A])(implicit F: Align[F]): Align.Ops[F, A] =
    new Align.Ops[F, A] {
      val self = fa
      val typeClassInstance = F
    }
}
