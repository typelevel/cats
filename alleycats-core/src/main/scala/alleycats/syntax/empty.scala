package alleycats
package syntax

import cats.Eq

object empty extends EmptySyntax

trait EmptySyntax {
  implicit class EmptyOps[A](a: A)(implicit ev: Empty[A]) {
    def isEmpty(implicit ev1: Eq[A]): Boolean = ev.isEmpty(a)
    def nonEmpty(implicit ev1: Eq[A]): Boolean = ev.nonEmpty(a)
  }
}
