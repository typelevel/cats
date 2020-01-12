package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * [[Semigroupal]] captures the idea of composing independent effectful values.
 * It is of particular interest when taken together with [[Functor]] - where [[Functor]]
 * captures the idea of applying a unary pure function to an effectful value,
 * calling `product` with `map` allows one to apply a function of arbitrary arity to multiple
 * independent effectful values.
 *
 * That same idea is also manifested in the form of [[Apply]], and indeed [[Apply]] extends both
 * [[Semigroupal]] and [[Functor]] to illustrate this.
 */
@implicitNotFound("Could not find an instance of Semigroupal for ${F}")
@typeclass trait Semigroupal[F[_]] extends Serializable {

  /**
   * Combine an `F[A]` and an `F[B]` into an `F[(A, B)]` that maintains the effects of both `fa` and `fb`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val noneInt: Option[Int] = None
   * scala> val some3: Option[Int] = Some(3)
   * scala> val noneString: Option[String] = None
   * scala> val someFoo: Option[String] = Some("foo")
   *
   * scala> Semigroupal[Option].product(noneInt, noneString)
   * res0: Option[(Int, String)] = None
   *
   * scala> Semigroupal[Option].product(noneInt, someFoo)
   * res1: Option[(Int, String)] = None
   *
   * scala> Semigroupal[Option].product(some3, noneString)
   * res2: Option[(Int, String)] = None
   *
   * scala> Semigroupal[Option].product(some3, someFoo)
   * res3: Option[(Int, String)] = Some((3,foo))
   * }}}
   */
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Semigroupal extends SemigroupalArityFunctions {

  /****************************************************************************
   * THE REST OF THIS OBJECT IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!! *
   ****************************************************************************/
  /**
   * Summon an instance of [[Semigroupal]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Semigroupal[F]): Semigroupal[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: Semigroupal[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def product[B](fb: F[B]): F[(A, B)] = typeClassInstance.product[A, B](self, fb)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToSemigroupalOps {
    implicit def toSemigroupalOps[F[_], A](target: F[A])(implicit tc: Semigroupal[F]): Ops[F, A] {
      type TypeClassType = Semigroupal[F]
    } = new Ops[F, A] {
      type TypeClassType = Semigroupal[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToSemigroupalOps
  object ops {
    implicit def toAllSemigroupalOps[F[_], A](target: F[A])(implicit tc: Semigroupal[F]): AllOps[F, A] {
      type TypeClassType = Semigroupal[F]
    } = new AllOps[F, A] {
      type TypeClassType = Semigroupal[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
}
