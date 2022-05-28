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

import cats.{CoflatMap, Comonad}

trait Extract[F[_]] extends Serializable {
  def extract[A](fa: F[A]): A
}

object Extract {
  // Ideally this would be an exported subclass instance provided by Comonad
  implicit def comonadIsExtract[F[_]](implicit ev: Comonad[F]): Extract[F] =
    new Extract[F] {
      def extract[A](fa: F[A]): A = ev.extract(fa)
    }

  // Ideally this would be an instance exported to Comonad
  implicit def extractCoflatMapIsComonad[F[_]](implicit e: Extract[F], cf: CoflatMap[F]): Comonad[F] =
    new Comonad[F] {
      def extract[A](fa: F[A]): A = e.extract(fa)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = cf.map(fa)(f)
      def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B] = cf.coflatMap(fa)(f)
    }

  /**
   * Summon an instance of [[Extract]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Extract[F]): Extract[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllExtractOps[F[_], A](target: F[A])(implicit tc: Extract[F]): AllOps[F, A] {
      type TypeClassType = Extract[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Extract[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Extract[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def extract: A = typeClassInstance.extract[A](self)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToExtractOps extends Serializable {
    implicit def toExtractOps[F[_], A](target: F[A])(implicit tc: Extract[F]): Ops[F, A] {
      type TypeClassType = Extract[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Extract[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToExtractOps

}
