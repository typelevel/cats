package cats
package syntax

import cats.instances.long._

trait UnorderedFoldableSyntax extends UnorderedFoldable.ToUnorderedFoldableOps {
  implicit final def catsSyntaxUnorderedFoldableOps[F[_]: UnorderedFoldable, A](fa: F[A]): UnorderedFoldableOps[F, A] =
    new UnorderedFoldableOps[F, A](fa)
}

final class UnorderedFoldableOps[F[_], A](val fa: F[A]) extends AnyVal {
  /**
    * Count the number of elements in the structure that satisfy the given predicate.
    *
    * For example:
    * {{{
    * scala> import cats.implicits._
    * scala> val map1 = Map[Int, String]()
    * scala> val p1: String => Boolean = _.length > 0
    * scala> map1.count(p1)
    * res0: Long = 0
    *
    * scala> val map2 = Map(1 -> "hello", 2 -> "world", 3 -> "!")
    * scala> val p2: String => Boolean = _.length > 1
    * scala> map2.count(p2)
    * res1: Long = 2
    * }}}
    */
  def count(p: A => Boolean)(implicit F: UnorderedFoldable[F]): Long =
    F.unorderedFoldMap(fa)(a => if (p(a)) 1L else 0L)
}
