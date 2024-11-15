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
package data

import cats.arrow.*
import cats.{CoflatMap, Comonad, Contravariant, Functor, Monad}

import scala.annotation.tailrec

/**
 * Represents a function `F[A] => B`.
 */
final case class Cokleisli[F[_], A, B](run: F[A] => B) { self =>

  /**
   * Example:
   * {{{
   * scala> import cats._, data._
   * scala> val f = Cokleisli((xs: NonEmptyList[Int]) => xs.reverse.head)
   * scala> def before(x: Double) = x.toInt
   * scala> def after(x: Int) = x.toString
   * scala> f.dimap(before)(after).run(NonEmptyList.of(1.0,2.0))
   * res0: String = 2
   * }}}
   */
  def dimap[C, D](f: C => A)(g: B => D)(implicit F: Functor[F]): Cokleisli[F, C, D] =
    Cokleisli(fc => g(run(F.map(fc)(f))))

  /**
   * Example:
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val f = Cokleisli((xs: NonEmptyList[Int]) => xs.reverse.head)
   * scala> def before(x: Double) = x.toInt
   * scala> def after(x: Int) = x.toString
   * scala> f.lmap(before).rmap(after).run(NonEmptyList.of(1.0,2.0))
   * res0: String = 2
   * }}}
   */
  def lmap[C](f: C => A)(implicit F: Functor[F]): Cokleisli[F, C, B] =
    Cokleisli(fc => run(F.map(fc)(f)))

  def map[C](f: B => C): Cokleisli[F, A, C] =
    Cokleisli(fa => f(run(fa)))

  /**
   * Example:
   * {{{
   * scala> import cats._, data._
   * scala> val sum = Cokleisli((xs: NonEmptyList[Int]) => xs.reduceLeft(_ + _))
   *
   * scala> sum.contramapValue((xs: NonEmptyList[String]) => xs.map(_.toInt)).run(NonEmptyList.of("1","2","3"))
   * res4: Int = 6
   * }}}
   */
  def contramapValue[C](f: F[C] => F[A]): Cokleisli[F, C, B] =
    Cokleisli(fc => run(f(fc)))

  def flatMap[C](f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
    Cokleisli(fa => f(self.run(fa)).run(fa))

  def compose[C](c: Cokleisli[F, C, A])(implicit F: CoflatMap[F]): Cokleisli[F, C, B] =
    Cokleisli(fc => run(F.coflatMap(fc)(c.run)))

  def andThen[C](c: Cokleisli[F, B, C])(implicit F: CoflatMap[F]): Cokleisli[F, A, C] =
    c.compose(this)

  def first[C](implicit F: Comonad[F]): Cokleisli[F, (A, C), (B, C)] =
    Cokleisli(fac => run(F.map(fac)(_._1)) -> F.extract(F.map(fac)(_._2)))

  def second[C](implicit F: Comonad[F]): Cokleisli[F, (C, A), (C, B)] =
    Cokleisli(fca => F.extract(F.map(fca)(_._1)) -> run(F.map(fca)(_._2)))
}

object Cokleisli extends CokleisliInstances {
  def pure[F[_], A, B](x: B): Cokleisli[F, A, B] =
    Cokleisli(_ => x)
}

sealed abstract private[data] class CokleisliInstances extends CokleisliInstances0 {

  implicit val catsDataCommutativeArrowForCokleisliId: CommutativeArrow[Cokleisli[Id, *, *]] =
    new CokleisliArrow[Id] with CommutativeArrow[Cokleisli[Id, *, *]] {
      def F: Comonad[Id] = Comonad[Id]
    }

  implicit def catsDataMonadForCokleisli[F[_], A]: Monad[Cokleisli[F, A, *]] =
    new CokleisliMonad[F, A]

  implicit def catsDataMonoidKForCokleisli[F[_]](implicit ev: Comonad[F]): MonoidK[λ[α => Cokleisli[F, α, α]]] =
    Category[Cokleisli[F, *, *]].algebraK
}

sealed abstract private[data] class CokleisliInstances0 extends CokleisliInstances1 {
  implicit def catsDataArrowForCokleisli[F[_]](implicit ev: Comonad[F]): Arrow[Cokleisli[F, *, *]] =
    new CokleisliArrow[F] { def F: Comonad[F] = ev }
}

sealed abstract private[data] class CokleisliInstances1 {
  implicit def catsDataComposeForCokleisli[F[_]](implicit ev: CoflatMap[F]): Compose[Cokleisli[F, *, *]] =
    new CokleisliCompose[F] { def F: CoflatMap[F] = ev }

  implicit def catsDataProfunctorForCokleisli[F[_]](implicit ev: Functor[F]): Profunctor[Cokleisli[F, *, *]] =
    new CokleisliProfunctor[F] { def F: Functor[F] = ev }

  implicit def catsDataSemigroupKForCokleisli[F[_]](implicit ev: CoflatMap[F]): SemigroupK[λ[α => Cokleisli[F, α, α]]] =
    Compose[Cokleisli[F, *, *]].algebraK

  implicit def catsDataContravariantForCokleisli[F[_]: Functor, A]: Contravariant[Cokleisli[F, *, A]] =
    new Contravariant[Cokleisli[F, *, A]] {
      def contramap[B, C](fbc: Cokleisli[F, B, A])(f: C => B): Cokleisli[F, C, A] = fbc.lmap(f)
    }
}

private[data] class CokleisliMonad[F[_], A] extends Monad[Cokleisli[F, A, *]] {

  def pure[B](x: B): Cokleisli[F, A, B] =
    Cokleisli.pure(x)

  def flatMap[B, C](fa: Cokleisli[F, A, B])(f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
    fa.flatMap(f)

  override def map[B, C](fa: Cokleisli[F, A, B])(f: B => C): Cokleisli[F, A, C] =
    fa.map(f)

  def tailRecM[B, C](b: B)(fn: B => Cokleisli[F, A, Either[B, C]]): Cokleisli[F, A, C] =
    Cokleisli { (fa: F[A]) =>
      @tailrec
      def loop(c: Cokleisli[F, A, Either[B, C]]): C =
        c.run(fa) match {
          case Right(c) => c
          case Left(bb) => loop(fn(bb))
        }
      loop(fn(b))
    }

}

private trait CokleisliArrow[F[_]]
    extends Arrow[Cokleisli[F, *, *]]
    with CokleisliCompose[F]
    with CokleisliProfunctor[F] {
  implicit def F: Comonad[F]

  def lift[A, B](f: A => B): Cokleisli[F, A, B] =
    Cokleisli(fa => f(F.extract(fa)))

  def first[A, B, C](fa: Cokleisli[F, A, B]): Cokleisli[F, (A, C), (B, C)] =
    fa.first[C]

  override def second[A, B, C](fa: Cokleisli[F, A, B]): Cokleisli[F, (C, A), (C, B)] =
    fa.second[C]

  override def dimap[A, B, C, D](fab: Cokleisli[F, A, B])(f: C => A)(g: B => D): Cokleisli[F, C, D] =
    super[CokleisliProfunctor].dimap(fab)(f)(g)

  override def split[A, B, C, D](f: Cokleisli[F, A, B], g: Cokleisli[F, C, D]): Cokleisli[F, (A, C), (B, D)] =
    Cokleisli(fac => f.run(F.map(fac)(_._1)) -> g.run(F.map(fac)(_._2)))
}

private trait CokleisliCompose[F[_]] extends Compose[Cokleisli[F, *, *]] {
  implicit def F: CoflatMap[F]

  def compose[A, B, C](f: Cokleisli[F, B, C], g: Cokleisli[F, A, B]): Cokleisli[F, A, C] =
    f.compose(g)
}

private trait CokleisliProfunctor[F[_]] extends Profunctor[Cokleisli[F, *, *]] {
  implicit def F: Functor[F]

  def dimap[A, B, C, D](fab: Cokleisli[F, A, B])(f: C => A)(g: B => D): Cokleisli[F, C, D] =
    fab.dimap(f)(g)

  override def lmap[A, B, C](fab: Cokleisli[F, A, B])(f: C => A): Cokleisli[F, C, B] =
    fab.lmap(f)

  override def rmap[A, B, C](fab: Cokleisli[F, A, B])(f: B => C): Cokleisli[F, A, C] =
    fab.map(f)
}
