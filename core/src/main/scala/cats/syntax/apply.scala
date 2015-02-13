package cats
package syntax

trait ApplySyntax {
  // TODO: use simulacrum instances eventually
  implicit def applySyntax[F[_]: Apply, A](fa: F[A]): ApplyOps[F, A] =
    new ApplyOps[F, A](fa)
}

class ApplyOps[F[_], A](fa: F[A])(implicit F: Apply[F]) {
  def apply[B](f: F[A => B]): F[B] = F.apply(fa)(f)
  def apply2[B, Z](fb: F[B])(f: F[(A, B) => Z]): F[Z] = F.apply2(fa, fb)(f)
  def map2[B, Z](fb: F[B])(f: (A, B) => Z): F[Z] = F.map2(fa, fb)(f)

  def |@|[B](fb: F[B]) = (new ApplyBuilder[F, A] {
    val a: F[A] = fa
  }) |@| fb

}

trait ApplyBuilder[F[_], A] {
  val a: F[A]
  @macros.apply.Builders trait ApplyBuilder2

  def |@|[B](b0: F[B]): ApplyBuilder2[B] = new ApplyBuilder2[B] {
    val b: F[B] = b0
  }

}

