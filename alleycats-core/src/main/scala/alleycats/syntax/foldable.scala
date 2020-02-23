package alleycats
package syntax

import cats.Foldable
import cats.syntax.foldable._

object foldable extends FoldableSyntax

trait FoldableSyntax {
  implicit class ExtraFoldableOps[F[_]: Foldable, A](fa: F[A]) {
    def foreach(f: A => Unit): Unit =
      fa.foldLeft(())((_, a) => f(a))
  }
}
