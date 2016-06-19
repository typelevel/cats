package cats

import simulacrum.typeclass

/**
 * Collect, also known as Witherable.
 *
 * TODO ceedumbs more info
 *
 * Based on Haskell's [[https://hackage.haskell.org/package/witherable-0.1.3.3/docs/Data-Witherable.html Data.Witherable]]
 */
@typeclass trait Collect[F[_]] extends Traverse[F] { self =>

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val m: Map[Int, String] = Map(1 -> "one", 3 -> "three")
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> def asString(i: Int): Eval[Option[String]] = Now(m.get(i))
   * scala> val result: Eval[List[String]] = l.mapOptionA(asString)
   * scala> result.value
   * res0: List[String] = List(one, three)
   * }}}
   */
  def mapOptionA[G[_]: Applicative, A, B](fa: F[A])(f: A => G[Option[B]]): G[F[B]]

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val m: Map[Int, String] = Map(1 -> "one", 3 -> "three")
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> def asString(i: Int): Option[String] = m.get(i)
   * scala> l.mapOption(i => m.get(i))
   * res0: List[String] = List(one, three)
   * }}}
   */
  def mapOption[A, B](fa: F[A])(f: A => Option[B]): F[B] =
    mapOptionA[Id, A, B](fa)(f)

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> Collect[List].collect(l){
   *      |   case 1 => "one"
   *      |   case 3 => "three"
   *      | }
   * res0: List[String] = List(one, three)
   * }}}
   */
  def collect[A, B](fa: F[A])(f: PartialFunction[A, B]): F[B] =
    mapOption(fa)(f.lift)

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Option[Int]] = List(Some(1), None, Some(3), None)
   * scala> l.flattenOption
   * res0: List[Int] = List(1, 3)
   * }}}
   */
  def flattenOption[A](fa: F[Option[A]]): F[A] = mapOption(fa)(identity)

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> def odd(i: Int): Eval[Boolean] = Now(i % 2 == 1)
   * scala> val res: Eval[List[Int]] = l.filterA(odd)
   * scala> res.value
   * res0: List[Int] = List(1, 3)
   * }}}
   */
  def filterA[G[_], A](fa: F[A])(f: A => G[Boolean])(implicit G: Applicative[G]): G[F[A]] =
    mapOptionA(fa)(a => G.map(f(a))(if (_) Some(a) else None))

  override def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    mapOptionA(fa)(a => G.map(f(a))(Some(_)))
}
