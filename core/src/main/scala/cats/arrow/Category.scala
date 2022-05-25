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
package arrow

/**
 * Must obey the laws defined in cats.laws.CategoryLaws.
 */
trait Category[F[_, _]] extends Compose[F] { self =>

  def id[A]: F[A, A]

  override def algebraK: MonoidK[λ[α => F[α, α]]] =
    new MonoidK[λ[α => F[α, α]]] {
      def empty[A]: F[A, A] = id
      def combineK[A](f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }

  override def algebra[A]: Monoid[F[A, A]] =
    new Monoid[F[A, A]] {
      def empty: F[A, A] = id
      def combine(f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }
}

object Category {

  /**
   * Summon an instance of [[Category]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Category[F]): Category[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllCategoryOps[F[_, _], A, B](target: F[A, B])(implicit tc: Category[F]): AllOps[F, A, B] {
      type TypeClassType = Category[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Category[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Category[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B] with Compose.AllOps[F, A, B] {
    type TypeClassType <: Category[F]
  }
  trait ToCategoryOps extends Serializable {
    implicit def toCategoryOps[F[_, _], A, B](target: F[A, B])(implicit tc: Category[F]): Ops[F, A, B] {
      type TypeClassType = Category[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Category[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToCategoryOps

}
