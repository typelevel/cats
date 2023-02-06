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

/**
 * `CoflatMap` is the dual of `FlatMap`.
 *
 * Must obey the laws in cats.laws.CoflatMapLaws
 */
trait CoflatMap[F[_]] extends Functor[F] {

  /**
   * `coflatMap` is the dual of `flatMap` on `FlatMap`. It applies
   * a value in a context to a function that takes a value
   * in a context and returns a normal value.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.CoflatMap
   * scala> val fa = Some(3)
   * scala> def f(a: Option[Int]): Int = a match {
   *      | case Some(x) => 2 * x
   *      | case None => 0 }
   * scala> CoflatMap[Option].coflatMap(fa)(f)
   * res0: Option[Int] = Some(6)
   * }}}
   */
  def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B]

  /**
   * `coflatten` is the dual of `flatten` on `FlatMap`. Whereas flatten removes
   * a layer of `F`, coflatten adds a layer of `F`
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.CoflatMap
   * scala> val fa = Some(3)
   * fa: Option[Int] = Some(3)
   * scala> CoflatMap[Option].coflatten(fa)
   * res0: Option[Option[Int]] = Some(Some(3))
   * }}}
   */
  def coflatten[A](fa: F[A]): F[F[A]] =
    coflatMap(fa)(fa => fa)
}

object CoflatMap {

  /**
   * Summon an instance of [[CoflatMap]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: CoflatMap[F]): CoflatMap[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllCoflatMapOps[F[_], A](target: F[A])(implicit tc: CoflatMap[F]): AllOps[F, A] {
      type TypeClassType = CoflatMap[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = CoflatMap[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: CoflatMap[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def coflatMap[B](f: F[A] => B): F[B] = typeClassInstance.coflatMap[A, B](self)(f)
    def coflatten: F[F[A]] = typeClassInstance.coflatten[A](self)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Functor.AllOps[F, A] {
    type TypeClassType <: CoflatMap[F]
  }
  trait ToCoflatMapOps extends Serializable {
    implicit def toCoflatMapOps[F[_], A](target: F[A])(implicit tc: CoflatMap[F]): Ops[F, A] {
      type TypeClassType = CoflatMap[F]
    } =
      new Ops[F, A] {
        type TypeClassType = CoflatMap[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToCoflatMapOps

}
