package cats
package syntax

trait ApplySyntax1 {
  implicit def applySyntaxU[A](a: A)(implicit U: Unapply[Apply, A]): ApplyOps[U.M, U.A] =
    new ApplyOps[U.M, U.A] {
      val self = U.subst(a)
      val typeClassInstance = U.TC
    }
}

trait ApplySyntax extends ApplySyntax1 {
  implicit def applySyntax[F[_], A](fa: F[A])(implicit F: Apply[F]): ApplyOps[F, A] =
    new ApplyOps[F,A] {
      val self = fa
      val typeClassInstance = F
    }
}

abstract class ApplyOps[F[_], A] extends Apply.Ops[F, A] {
  def |@|[B](fb: F[B]) = new ApplyBuilder[F] |@| self |@| fb

  /**
   * combine both contexts but only return the right value
   */
  def *>[B](fb: F[B]) = typeClassInstance.map2(self, fb)((a,b) => b)

  /**
   * combine both contexts but only return the left value
   */
  def <*[B](fb: F[B]) = typeClassInstance.map2(self, fb)((a,b) => a)
}
