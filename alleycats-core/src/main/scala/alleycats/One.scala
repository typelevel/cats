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

import cats.Eq
import cats.syntax.eq.*

trait One[A] extends Serializable {
  def one: A

  def isOne(a: A)(implicit ev: Eq[A]): Boolean =
    one === a

  def nonOne(a: A)(implicit ev: Eq[A]): Boolean =
    one =!= a
}

object One {
  def apply[A](a: => A): One[A] =
    new One[A] { lazy val one: A = a }

  /**
   * Summon an instance of [[One]] for `A`.
   */
  @inline def apply[A](implicit instance: One[A]): One[A] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllOneOps[A](target: A)(implicit tc: One[A]): AllOps[A] {
      type TypeClassType = One[A]
    } =
      new AllOps[A] {
        type TypeClassType = One[A]
        val self: A = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[A] extends Serializable {
    type TypeClassType <: One[A]
    def self: A
    val typeClassInstance: TypeClassType
    def isOne(implicit ev: Eq[A]): Boolean = typeClassInstance.isOne(self)(ev)
    def nonOne(implicit ev: Eq[A]): Boolean = typeClassInstance.nonOne(self)(ev)
  }
  trait AllOps[A] extends Ops[A]
  trait ToOneOps extends Serializable {
    implicit def toOneOps[A](target: A)(implicit tc: One[A]): Ops[A] {
      type TypeClassType = One[A]
    } =
      new Ops[A] {
        type TypeClassType = One[A]
        val self: A = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToOneOps

}
