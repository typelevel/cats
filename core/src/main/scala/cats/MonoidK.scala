/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats

import cats.kernel.compat.scalaVersionSpecific.*

/**
 * MonoidK is a universal monoid which operates on kinds.
 *
 * This type class is useful when its type parameter F[_] has a
 * structure that can be combined for any particular type, and which
 * also has an "empty" representation. Thus, MonoidK is like a Monoid
 * for kinds (i.e. parametrized types).
 *
 * A MonoidK[F] can produce a Monoid[F[A]] for any type A.
 *
 * Here's how to distinguish Monoid and MonoidK:
 *
 *  - Monoid[A] allows A values to be combined, and also means there
 *    is an "empty" A value that functions as an identity.
 *
 *  - MonoidK[F] allows two F[A] values to be combined, for any A.  It
 *    also means that for any A, there is an "empty" F[A] value. The
 *    combination operation and empty value just depend on the
 *    structure of F, but not on the structure of A.
 */
trait MonoidK[F[_]] extends SemigroupK[F] { self =>

  /**
   * Given a type A, create an "empty" F[A] value.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> MonoidK[List].empty[Long]
   * res0: List[Long] = List()
   * }}}
   */
  def empty[A]: F[A]

  /**
   * Tests if `a` is the identity.
   *
   * Example:
   * {{{
   * scala> MonoidK[List].isEmpty(List.empty[String])
   * res0: Boolean = true
   *
   * scala> MonoidK[List].isEmpty(List("something"))
   * res1: Boolean = false
   * }}}
   */
  def isEmpty[A](a: F[A])(implicit ev: Eq[F[A]]): Boolean =
    ev.eqv(a, empty)

  /**
   * Given a type A, create a concrete Monoid[F[A]].
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> MonoidK[List].algebra[Long].empty
   * res0: List[Long] = List()
   * }}}
   */
  override def algebra[A]: Monoid[F[A]] =
    new Monoid[F[A]] {
      def empty: F[A] = self.empty
      def combine(x: F[A], y: F[A]): F[A] = self.combineK(x, y)
    }

  /**
   * Given a kind G, create an "composed" MonoidK[F[G[_]]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val monoidK = MonoidK[List].compose[Option]
   * scala> monoidK.combineK(List(Some(1)), List(Some(2), None))
   * res0: List[Option[Int]] = List(Some(1), Some(2), None)
   * }}}
   */
  override def compose[G[_]]: MonoidK[λ[α => F[G[α]]]] =
    new ComposedMonoidK[F, G] {
      val F: MonoidK[F] = self
    }

  /**
   * Return `a` combined with itself `n` times.
   *
   * Example:
   * {{{
   * scala> SemigroupK[List].combineNK(List(1), 5)
   * res0: List[Int] = List(1, 1, 1, 1, 1)

   * scala> MonoidK[List].combineNK(List("ha"), 0)
   * res1: List[String] = List()
   *
   * }}}
   */
  override def combineNK[A](a: F[A], n: Int): F[A] =
    if (n < 0) throw new IllegalArgumentException("Repeated combining for monoidKs must have n >= 0")
    else if (n == 0) empty[A]
    else repeatedCombineNK(a, n)

  /**
   * Given a sequence of `as`, sum them using the monoidK and return the total.
   *
   * Example:
   * {{{
   * scala> MonoidK[List].combineAllK(List(List("One"), List("Two"), List("Three")))
   * res0: List[String] = List(One, Two, Three)
   *
   * scala> MonoidK[List].combineAllK[String](List.empty)
   * res1: List[String] = List()
   * }}}
   */
  def combineAllK[A](as: IterableOnce[F[A]]): F[A] =
    combineAllOptionK(as) match {
      case Some(fa) => fa
      case None     => empty[A]
    }

  override def reverse: MonoidK[F] =
    new MonoidK[F] {
      def empty[A] = self.empty
      def combineK[A](a: F[A], b: F[A]) = self.combineK(b, a)
      // a + a + a + ... is the same when reversed
      override def combineNK[A](a: F[A], n: Int): F[A] = self.combineNK(a, n)
      override def reverse = self
    }
}

@suppressUnusedImportWarningForScalaVersionSpecific
object MonoidK {

  /**
   * Summon an instance of [[MonoidK]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: MonoidK[F]): MonoidK[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllMonoidKOps[F[_], A](target: F[A])(implicit tc: MonoidK[F]): AllOps[F, A] {
      type TypeClassType = MonoidK[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = MonoidK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: MonoidK[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with SemigroupK.AllOps[F, A] {
    type TypeClassType <: MonoidK[F]
  }
  trait ToMonoidKOps extends Serializable {
    implicit def toMonoidKOps[F[_], A](target: F[A])(implicit tc: MonoidK[F]): Ops[F, A] {
      type TypeClassType = MonoidK[F]
    } =
      new Ops[F, A] {
        type TypeClassType = MonoidK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToMonoidKOps

}
