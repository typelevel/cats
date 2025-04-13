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

import cats.{Eq, Monoid}
import cats.syntax.eq.*

import scala.collection.immutable.SortedMap

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

  private[this] val emptyOptionSingleton: Empty[Option[Nothing]] = Empty(None)
  implicit def alleycatsEmptyForOption[A]: Empty[Option[A]] = emptyOptionSingleton.asInstanceOf[Empty[Option[A]]]

  private[this] val emptyMapSingleton: Empty[Map[Nothing, Nothing]] = Empty(Map.empty)
  implicit def alleycatsEmptyForMap[A, B]: Empty[Map[A, B]] = emptyMapSingleton.asInstanceOf[Empty[Map[A, B]]]

  implicit def alleycatsEmptyForSortedMap[A: Ordering, B]: Empty[SortedMap[A, B]] =
    Empty(SortedMap.empty[A, B])
}

private[alleycats] trait EmptyInstances1 extends EmptyInstances2 {
  // If Monoid extended Empty then this could be an exported subclass instance provided by Monoid
  implicit def monoidIsEmpty[A: Monoid]: Empty[A] =
    Empty(Monoid[A].empty)
}

private[alleycats] trait EmptyInstances2 {
  implicit def fromEmptyK[F[_], T](implicit ekf: EmptyK[F]): Empty[F[T]] = ekf.synthesize[T]
}
