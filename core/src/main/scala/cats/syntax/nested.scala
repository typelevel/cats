package cats
package syntax

import cats.data.Nested

trait NestedSyntax {
  implicit final def catsSyntaxNestedId[F[_], G[_], A](value: F[G[A]]): NestedIdOps[F, G, A] =
    new NestedIdOps[F, G, A](value)
}

final class NestedIdOps[F[_], G[_], A](private[syntax] val value: F[G[A]]) extends AnyVal {

  /**
   * Wrap a value in `Nested`.
   *
   * `x.nested` is equivalent to `Nested(x)`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> List(Some(3), None).nested.map(_+1).value
   * res0: List[Option[Int]] = List(Some(4), None)
   * }}}
   */
  def nested: Nested[F, G, A] = Nested[F, G, A](value)
}
