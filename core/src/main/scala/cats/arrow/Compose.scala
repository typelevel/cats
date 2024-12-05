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
 * Must obey the laws defined in cats.laws.ComposeLaws.
 *
 * Here's how you can use `>>>` and `<<<`
 * Example:
 * {{{
 * scala> import cats.syntax.all._
 * scala> val f : Int => Int = (_ + 1)
 * scala> val g : Int => Int = (_ * 100)
 * scala> (f >>> g)(3)
 * res0: Int = 400
 * scala> (f <<< g)(3)
 * res1: Int = 301
 * }}}
 */
trait Compose[F[_, _]] extends Serializable { self =>

  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]

  def andThen[A, B, C](f: F[A, B], g: F[B, C]): F[A, C] =
    compose(g, f)

  def algebraK: SemigroupK[λ[α => F[α, α]]] =
    new SemigroupK[λ[α => F[α, α]]] {
      def combineK[A](f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }

  def algebra[A]: Semigroup[F[A, A]] = compose(_, _)
}

object Compose {
  implicit def catsInstancesForFunction1: ArrowChoice[Function1] & CommutativeArrow[Function1] =
    cats.instances.function.catsStdInstancesForFunction1
  implicit def catsComposeForMap: Compose[Map] = cats.instances.map.catsStdComposeForMap

  implicit def catsInstancesForPartialFunction: ArrowChoice[PartialFunction] & CommutativeArrow[PartialFunction] =
    cats.instances.partialFunction.catsStdInstancesForPartialFunction

  /**
   * Summon an instance of [[Compose]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Compose[F]): Compose[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllComposeOps[F[_, _], A, B](target: F[A, B])(implicit tc: Compose[F]): AllOps[F, A, B] {
      type TypeClassType = Compose[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Compose[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Compose[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def compose[C](g: F[C, A]): F[C, B] = typeClassInstance.compose[C, A, B](self, g)
    def <<<[C](g: F[C, A]): F[C, B] = typeClassInstance.compose[C, A, B](self, g)
    def andThen[C](g: F[B, C]): F[A, C] = typeClassInstance.andThen[A, B, C](self, g)
    def >>>[C](g: F[B, C]): F[A, C] = typeClassInstance.andThen[A, B, C](self, g)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B]
  trait ToComposeOps extends Serializable {
    implicit def toComposeOps[F[_, _], A, B](target: F[A, B])(implicit tc: Compose[F]): Ops[F, A, B] {
      type TypeClassType = Compose[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Compose[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToComposeOps

}
