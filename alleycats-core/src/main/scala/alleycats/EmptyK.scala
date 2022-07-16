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

trait EmptyK[F[_]] extends Serializable { self =>
  def empty[A]: F[A]

  def synthesize[A]: Empty[F[A]] =
    new Empty[F[A]] {
      def empty: F[A] = self.empty[A]
    }
}

object EmptyK extends EmptyKInstances0 {

  /**
   * Summon an instance of [[EmptyK]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: EmptyK[F]): EmptyK[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllEmptyKOps[F[_], A](target: F[A])(implicit tc: EmptyK[F]): AllOps[F, A] {
      type TypeClassType = EmptyK[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = EmptyK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: EmptyK[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToEmptyKOps extends Serializable {
    implicit def toEmptyKOps[F[_], A](target: F[A])(implicit tc: EmptyK[F]): Ops[F, A] {
      type TypeClassType = EmptyK[F]
    } =
      new Ops[F, A] {
        type TypeClassType = EmptyK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToEmptyKOps

}

private[alleycats] trait EmptyKInstances0 {
  implicit def alleycatsEmptyKForMap[K]: EmptyK[Map[K, *]] = alleycats.std.map.alletcatsStdMapEmptyK[K]
}
