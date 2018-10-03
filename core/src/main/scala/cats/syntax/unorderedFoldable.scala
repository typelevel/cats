package cats
package syntax

import cats.instances.long._
import cats.kernel.CommutativeMonoid

trait UnorderedFoldableSyntax extends UnorderedFoldable.ToUnorderedFoldableOps {
  implicit final def catsSyntaxUnorderedFoldableOps[F[_]: UnorderedFoldable, A](fa: F[A]): UnorderedFoldableOps[F, A] =
    new UnorderedFoldableOps[F, A](fa)
}

final class UnorderedFoldableOps[F[_], A](val fa: F[A]) extends AnyVal {
  private def orderMaxCommutativeMonoidBy[B](f: A => B)(implicit ev: Order[B]): CommutativeMonoid[Option[A]] =
    new CommutativeMonoid[Option[A]] {
      override def empty = None
      override def combine(optX: Option[A], optY: Option[A]): Option[A] = (optX, optY) match {
        case (None, y) => y
        case (x, None) => x
        case (sx @ Some(x), sy @ Some(y)) => if (ev.gteqv(f(x), f(y))) sx else sy
      }
    }

  private def orderMinCommutativeMonoidBy[B](f: A => B)(implicit ev: Order[B]): CommutativeMonoid[Option[A]] =
    new CommutativeMonoid[Option[A]] {
      override def empty = None
      override def combine(optX: Option[A], optY: Option[A]): Option[A] = (optX, optY) match {
        case (None, y) => y
        case (x, None) => x
        case (sx @ Some(x), sy @ Some(y)) => if (ev.lteqv(f(x), f(y))) sx else sy
      }
    }

  /**
    * Find the maximum item in this structure according to the given function.
    *
    * If there are no elements, the result is `None`.
    *
    * For example:
    * {{{
    * scala> import cats.implicits._
    * scala> val set1 = Set[String]()
    * scala> set1.maxByOption(_.length)
    * res0: Option[String] = None
    *
    * scala> val set2 = Set("Two", "Three", "Four")
    * scala> set2.maxByOption(_.length)
    * res1: Option[String] = Some(Three)
    *
    * scala> val set3 = Set(41, 32, 23)
    * scala> set3.maxByOption(_ % 10)
    * res2: Option[Int] = Some(23)
    * }}}
    */
  def maxByOption[B](f: A => B)(implicit F: UnorderedFoldable[F], B: Order[B]): Option[A] =
    F.unorderedFoldMap(fa)(Some.apply: A => Option[A])(orderMaxCommutativeMonoidBy(f))

  /**
    * Find the minimum item in this structure according to the given function.
    *
    * If there are no elements, the result is `None`.
    *
    * For example:
    * {{{
    * scala> import cats.implicits._
    * scala> val set1 = Set[String]()
    * scala> set1.minByOption(_.length)
    * res0: Option[String] = None
    *
    * scala> val set2 = Set[String]("Two", "Three", "Four")
    * scala> set2.minByOption(_.length)
    * res1: Option[String] = Some(Two)
    *
    * scala> val set3 = Set[Int](41, 32, 23)
    * scala> set3.minByOption(_ % 10)
    * res2: Option[Int] = Some(41)
    * }}}
    */
  def minByOption[B](f: A => B)(implicit F: UnorderedFoldable[F], B: Order[B]): Option[A] =
    F.unorderedFoldMap(fa)(Some.apply: A => Option[A])(orderMinCommutativeMonoidBy(f))

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
