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

package alleycats

import cats.{Eq, Monoid, MonoidK}
import cats.syntax.eq._

trait Empty[A] extends Serializable {
  def empty: A

  def isEmpty(a: A)(implicit ev: Eq[A]): Boolean =
    empty === a

  def nonEmpty(a: A)(implicit ev: Eq[A]): Boolean =
    empty =!= a
}

object Empty extends EmptyInstances0 {
  def apply[A](a: => A): Empty[A] =
    new Empty[A] { lazy val empty: A = a }

  def fromEmptyK[F[_], T](implicit ekf: EmptyK[F]): Empty[F[T]] = ekf.synthesize[T]

  /**
   * Summon an instance of [[Empty]] for `A`.
   */
  @inline def apply[A](implicit instance: Empty[A]): Empty[A] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllEmptyOps[A](target: A)(implicit tc: Empty[A]): AllOps[A] {
      type TypeClassType = Empty[A]
    } =
      new AllOps[A] {
        type TypeClassType = Empty[A]
        val self: A = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[A] extends Serializable {
    type TypeClassType <: Empty[A]
    def self: A
    val typeClassInstance: TypeClassType
    def isEmpty(implicit ev: Eq[A]): Boolean = typeClassInstance.isEmpty(self)(ev)
    def nonEmpty(implicit ev: Eq[A]): Boolean = typeClassInstance.nonEmpty(self)(ev)
  }
  trait AllOps[A] extends Ops[A]
  trait ToEmptyOps extends Serializable {
    implicit def toEmptyOps[A](target: A)(implicit tc: Empty[A]): Ops[A] {
      type TypeClassType = Empty[A]
    } =
      new Ops[A] {
        type TypeClassType = Empty[A]
        val self: A = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToEmptyOps

}

private[alleycats] trait EmptyInstances0 extends compat.IterableEmptyInstance with EmptyInstances1 {
  implicit def monoidKIsEmpty[F[_]: MonoidK, A]: Empty[F[A]] = Empty(MonoidK[F].empty[A])
}

private[alleycats] trait EmptyInstances1 {
  // If Monoid extended Empty then this could be an exported subclass instance provided by Monoid
  implicit def monoidIsEmpty[A: Monoid]: Empty[A] =
    Empty(Monoid[A].empty)
}
