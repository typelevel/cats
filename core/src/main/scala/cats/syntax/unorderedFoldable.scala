package cats
package syntax

import cats.instances.long._

trait UnorderedFoldableSyntax extends UnorderedFoldable.ToUnorderedFoldableOps {
  implicit final def catsSyntaxUnorderedFoldableOps[F[_]: UnorderedFoldable, A](fa: F[A]): UnorderedFoldableOps[F, A] =
    new UnorderedFoldableOps[F, A](fa)
}

trait UnorderedFoldableSyntaxBinCompat0 {
  implicit final def catsSyntaxUnorderedFoldableOps0[F[_]: UnorderedFoldable, A](fa: F[A]): UnorderedFoldableOps0[F, A] =
    new UnorderedFoldableOps0[F, A](fa)
}

final class UnorderedFoldableOps[F[_], A](private val fa: F[A]) extends AnyVal {

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

final class UnorderedFoldableOps0[F[_], A](private val fa: F[A]) extends AnyVal {

  /**
   * test if `F[A]` contains an `A`, named contains_ to avoid conflict with existing contains which uses universal equality
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val s: Set[Int] = Set(1, 2, 3, 4)
   * scala> s.contains_(1)
   * res0: Boolean = true
   * scala> s.contains_(5)
   * res1: Boolean = false
   * }}}
   */
  def contains_(v: A)(implicit ev: Eq[A], F: UnorderedFoldable[F]): Boolean =
    F.exists(fa)(ev.eqv(_, v))
}
