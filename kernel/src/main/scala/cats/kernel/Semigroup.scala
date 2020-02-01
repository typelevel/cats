package cats.kernel

import scala.{specialized => sp}
import scala.annotation.tailrec
import compat.scalaVersionSpecific._

/**
 * A semigroup is any set `A` with an associative operation (`combine`).
 */
trait Semigroup[@sp(Int, Long, Float, Double) A] extends Any with Serializable { self =>

  /**
   * Associative operation which combines two values.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.option._
   *
   * scala> Semigroup[String].combine("Hello ", "World!")
   * res0: String = Hello World!
   *
   * scala> Semigroup[Option[Int]].combine(None, Some(1))
   * res1: Option[Int] = Some(1)
   * }}}
   */
  def combine(x: A, y: A): A

  /**
   * Return `a` combined with itself `n` times.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.string._
   *
   * scala> Semigroup[Int].combineN(1, 10)
   * res0: Int = 10
   *
   * scala> Semigroup[String].combineN("ha", 3)
   * res1: String = hahaha
   * }}}
   */
  def combineN(a: A, n: Int): A =
    if (n <= 0) throw new IllegalArgumentException("Repeated combining for semigroups must have n > 0")
    else repeatedCombineN(a, n)

  /**
   * Return `a` combined with itself more than once.
   */
  protected[this] def repeatedCombineN(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) combine(b, extra)
      else {
        val x = if ((k & 1) == 1) combine(b, extra) else extra
        loop(combine(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

  /**
   * Given a sequence of `as`, combine them and return the total.
   *
   * If the sequence is empty, returns None. Otherwise, returns Some(total).
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Semigroup[String].combineAllOption(List("One ", "Two ", "Three"))
   * res0: Option[String] = Some(One Two Three)
   *
   * scala> Semigroup[String].combineAllOption(List.empty)
   * res1: Option[String] = None
   * }}}
   */
  def combineAllOption(as: IterableOnce[A]): Option[A] =
    as.reduceOption(combine)

  /**
   * return a semigroup that reverses the order
   * so combine(a, b) == reverse.combine(b, a)
   */
  def reverse: Semigroup[A] =
    new Semigroup[A] {
      def combine(a: A, b: A): A = self.combine(b, a)
      // a + a + a + ... is the same when reversed
      override def combineN(a: A, n: Int): A = self.combineN(a, n)
      override def reverse = self
    }

  /**
   * Between each pair of elements insert middle
   * This name matches the term used in Foldable and Reducible and a similar Haskell function.
   */
  def intercalate(middle: A): Semigroup[A] =
    new Semigroup[A] {
      def combine(a: A, b: A): A =
        self.combine(a, self.combine(middle, b))
    }
}

abstract class SemigroupFunctions[S[T] <: Semigroup[T]] {
  def combine[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: S[A]): A =
    ev.combine(x, y)

  def maybeCombine[@sp(Int, Long, Float, Double) A](ox: Option[A], y: A)(implicit ev: S[A]): A =
    ox match {
      case Some(x) => ev.combine(x, y)
      case None    => y
    }

  def maybeCombine[@sp(Int, Long, Float, Double) A](x: A, oy: Option[A])(implicit ev: S[A]): A =
    oy match {
      case Some(y) => ev.combine(x, y)
      case None    => x
    }

  def isCommutative[A](implicit ev: S[A]): Boolean =
    ev.isInstanceOf[CommutativeSemigroup[_]]

  def isIdempotent[A](implicit ev: S[A]): Boolean =
    ev.isInstanceOf[Band[_]]

  def combineN[@sp(Int, Long, Float, Double) A](a: A, n: Int)(implicit ev: S[A]): A =
    ev.combineN(a, n)

  def combineAllOption[A](as: IterableOnce[A])(implicit ev: S[A]): Option[A] =
    ev.combineAllOption(as)
}

object Semigroup extends SemigroupFunctions[Semigroup] {

  /**
   * Access an implicit `Semigroup[A]`.
   */
  @inline final def apply[A](implicit ev: Semigroup[A]): Semigroup[A] = ev

  /**
   * Create a `Semigroup` instance from the given function.
   */
  @inline def instance[A](cmb: (A, A) => A): Semigroup[A] = new Semigroup[A] {
    override def combine(x: A, y: A): A = cmb(x, y)
  }
}
