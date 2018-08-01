package cats.kernel

import scala.{ specialized => sp }
import scala.annotation.tailrec

/**
  * A magma is any set `A` with a binary operation (`combine`).
  */
trait Magma[@sp(Int, Long, Float, Double) A] extends Any with Serializable {

  /**
    * Binary operation which combines two values.
    */
  def combine(x: A, y: A): A
  
  /**
    * Return `a` combined with itself `n` times.
    */
  def combineN(a: A, n: Int): A =
    if (n <= 0) throw new IllegalArgumentException("Repeated combining for magmas must have n > 0")
    else repeatedCombineN(a, n)

  /**
    * Return `a` combined with itself more than once.
    */
  protected[this] def repeatedCombineN(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) combine(b, extra) else {
        val x = if ((k & 1) == 1) combine(b, extra) else extra
        loop(combine(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

  /**
    * Given a sequence of `as`, combine them and return the total.
    *
    * If the sequence is empty, returns None. Otherwise, returns Some(total).
    */
  def combineAllOption(as: TraversableOnce[A]): Option[A] =
    as.reduceOption(combine)
}

abstract class MagmaFunctions[M[T] <: Magma[T]] {

  def combine[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: M[A]): A =
    ev.combine(x, y)

  def maybeCombine[@sp(Int, Long, Float, Double) A](ox: Option[A], y: A)(implicit ev: M[A]): A =
    ox match {
      case Some(x) => ev.combine(x, y)
      case None => y
    }

  def maybeCombine[@sp(Int, Long, Float, Double) A](x: A, oy: Option[A])(implicit ev: M[A]): A =
    oy match {
      case Some(y) => ev.combine(x, y)
      case None => x
    }

  def isCommutative[A](implicit ev: M[A]): Boolean =
  ev.isInstanceOf[CommutativeSemigroup[_]]

  def isIdempotent[A](implicit ev: M[A]): Boolean =
  ev.isInstanceOf[Band[_]]

  def combineN[@sp(Int, Long, Float, Double) A](a: A, n: Int)(implicit ev: M[A]): A =
  ev.combineN(a, n)

  def combineAllOption[A](as: TraversableOnce[A])(implicit ev: M[A]): Option[A] =
  ev.combineAllOption(as)

}

object Magma extends MagmaFunctions[Magma] {

  /**
    * Access an implicit `Magma[A]`.
    */
  @inline final def apply[A](implicit ev: Magma[A]): Magma[A] = ev

  /**
    * Create a `Magma` instance from the given function.
    */
  @inline def instance[A](cmb: (A, A) => A): Magma[A] = new Magma[A] {
    override def combine(x: A, y: A): A = cmb(x, y)
  }
}
