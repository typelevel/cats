package cats
package syntax

import cats.data.Streaming
import cats.data.Streaming.Cons

/**
 * Contains various Stream-specific syntax.
 *
 * To eanble this, do one of the following:
 *
 *   import cats.implicits._
 *   import cats.syntax.all._
 *   import cats.syntax.streaming._
 *
 * This provides the %:: and %::: operators for constructing Streams
 * lazily, and the %:: extract to use when pattern matching on
 * Streams.
 */
trait StreamingSyntax {
  object %:: {
    def unapply[A](s: Streaming[A]): Option[(A, Eval[Streaming[A]])] = s.uncons
  }

  implicit def streamingOps[A](as: => Streaming[A]): StreamingOps[A] =
    new StreamingOps(Always(as))

  final class StreamingOps[A](rhs: Eval[Streaming[A]]) {
    def %::(lhs: A): Streaming[A] = Cons(lhs, rhs)
    def %:::(lhs: Streaming[A]): Streaming[A] = lhs ++ rhs
  }
}
