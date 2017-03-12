package cats

import simulacrum.typeclass

/**
 * `CoflatMap` is the dual of `FlatMap`.
 *
 * Must obey the laws in cats.laws.CoflatMapLaws
 */
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
