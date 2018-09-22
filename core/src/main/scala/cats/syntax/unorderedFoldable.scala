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
    * scala> val set1 = Set[String]()
    * scala> set1.count_(_.length > 0)
    * res0: Long = 0
    *
    * scala> val set2 = Set("hello", "world", "!")
    * scala> set2.count_(_.length > 1)
    * res1: Long = 2
    *
    * scala> val set3 = Set(41, 32, 23)
    * scala> set3.count_(_ % 2 == 0)
    * res2: Long = 1
    * }}}
    */
  def count_(p: A => Boolean)(implicit F: UnorderedFoldable[F]): Long =
    F.unorderedFoldMap(fa)(a => if (p(a)) 1L else 0L)
}
