package cats
package syntax

import cats.data.Coproduct

trait CoproductSyntax {
  implicit def coproductSyntax[F[_], A](a: F[A]): CoproductOps[F, A] = new CoproductOps(a)
}

final class CoproductOps[F[_], A](val a: F[A]) extends AnyVal {
  def leftc[G[_]]: Coproduct[F, G, A] = Coproduct.leftc(a)
  def rightc[G[_]]: Coproduct[G, F, A] = Coproduct.rightc(a)
}
