package cats

import simulacrum.typeclass

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
@typeclass trait Semigroupal[F[_]] {

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

object Semigroupal extends SemigroupalArityFunctions
