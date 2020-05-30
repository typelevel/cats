package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * `CoflatMap` is the dual of `FlatMap`.
 *
 * Must obey the laws in cats.laws.CoflatMapLaws
 */
@implicitNotFound("Could not find an instance of CoflatMap for ${F}")
@typeclass trait CoflatMap[F[_]] extends Functor[F] {

  /**
   * `coflatMap` is the dual of `flatMap` on `FlatMap`. It applies
   * a value in a context to a function that takes a value
   * in a context and returns a normal value.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
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
   * scala> import cats.implicits._
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

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/
  /**
   * Summon an instance of [[CoflatMap]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: CoflatMap[F]): CoflatMap[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: CoflatMap[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def coflatMap[B](f: F[A] => B): F[B] = typeClassInstance.coflatMap[A, B](self)(f)
    def coflatten: F[F[A]] = typeClassInstance.coflatten[A](self)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Functor.AllOps[F, A] {
    type TypeClassType <: CoflatMap[F]
  }
  trait ToCoflatMapOps {
    implicit def toCoflatMapOps[F[_], A](target: F[A])(implicit tc: CoflatMap[F]): Ops[F, A] {
      type TypeClassType = CoflatMap[F]
    } =
      new Ops[F, A] {
        type TypeClassType = CoflatMap[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  object nonInheritedOps extends ToCoflatMapOps
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

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
