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

import cats.kernel.compat.scalaVersionSpecific.*

trait Alternative[F[_]] extends NonEmptyAlternative[F] with MonoidK[F] { self =>

  // Note: `protected` is only necessary to enforce binary compatibility
  // since neither `private` nor `private[cats]` work properly here.
  @deprecated("use a FlatMap-constrained version instead", "2.6.2")
  protected def unite[G[_], A](fga: F[G[A]])(FM: Monad[F], G: Foldable[G]): F[A] = {
    implicit def FM0: FlatMap[F] = FM
    implicit def G0: Foldable[G] = G
    unite(fga)
  }

  /**
   * Fold over the inner structure to combine all of the values with
   * our combine method inherited from MonoidK. The result is for us
   * to accumulate all of the "interesting" values of the inner G, so
   * if G is Option, we collect all the Some values, if G is Either,
   * we collect all the Right values, etc.
   *
   * Example:
   * {{{
   * scala> val x: List[Vector[Int]] = List(Vector(1, 2), Vector(3, 4))
   * scala> Alternative[List].unite(x)
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def unite[G[_], A](fga: F[G[A]])(implicit FM: FlatMap[F], G: Foldable[G]): F[A] =
    FM.flatMap(fga) { G.foldMapK(_)(pure)(self) }

  // Note: `protected` is only necessary to enforce binary compatibility
  // since neither `private` nor `private[cats]` work properly here.
  @deprecated("use a FlatMap-constrained version instead", "2.6.2")
  protected def separate[G[_, _], A, B](fgab: F[G[A, B]])(FM: Monad[F], G: Bifoldable[G]): (F[A], F[B]) = {
    implicit def FM0: FlatMap[F] = FM
    implicit def G0: Bifoldable[G] = G
    separate(fgab)
  }

  /**
   * Separate the inner foldable values into the "lefts" and "rights".
   *
   * Example:
   * {{{
   * scala> val l: List[Either[String, Int]] = List(Right(1), Left("error"))
   * scala> Alternative[List].separate(l)
   * res0: (List[String], List[Int]) = (List(error),List(1))
   * }}}
   */
  def separate[G[_, _], A, B](fgab: F[G[A, B]])(implicit FM: FlatMap[F], G: Bifoldable[G]): (F[A], F[B]) = {
    val as = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(pure, _ => empty[A])(algebra[A]))
    val bs = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(_ => empty[B], pure)(algebra[B]))
    (as, bs)
  }

  /**
   * Separate the inner foldable values into the "lefts" and "rights".
   *
   * A variant of [[[separate[G[_,_],A,B](fgab:F[G[A,B]])(implicitFM:cats\.FlatMap[F]* separate]]]
   * that is specialized for Fs that have Foldable instances which allows for a single-pass implementation
   * (as opposed to {{{separate}}} which is 2-pass).
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val l: List[Either[String, Int]] = List(Right(1), Left("error"))
   * scala> Alternative[List].separateFoldable(l)
   * res0: (List[String], List[Int]) = (List(error),List(1))
   * }}}
   */
  def separateFoldable[G[_, _], A, B](fgab: F[G[A, B]])(implicit G: Bifoldable[G], FF: Foldable[F]): (F[A], F[B]) =
    FF.foldLeft(fgab, (empty[A], empty[B])) { case (mamb, gab) =>
      G.bifoldLeft(gab, mamb)(
        (t, a) => (appendK(t._1, a), t._2),
        (t, b) => (t._1, appendK(t._2, b))
      )
    }

  /**
   * Return ().pure[F] if `condition` is true, `empty` otherwise
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> def even(i: Int): Option[String] = Alternative[Option].guard(i % 2 == 0).as("even")
   * scala> even(2)
   * res0: Option[String] = Some(even)
   * scala> even(3)
   * res1: Option[String] = None
   * }}}
   */
  def guard(condition: Boolean): F[Unit] =
    if (condition) unit else empty

  override def compose[G[_]: Applicative]: Alternative[λ[α => F[G[α]]]] =
    new ComposedAlternative[F, G] {
      val F = self
      val G = Applicative[G]
    }

  def fromIterableOnce[A](as: IterableOnce[A]): F[A] =
    combineAllK(as.iterator.map(pure(_)))

  final def fromFoldable[G[_]: Foldable, A](as: G[A]): F[A] =
    fromIterableOnce(Foldable[G].toIterable(as))
}

@suppressUnusedImportWarningForScalaVersionSpecific
object Alternative {

  /**
   * Summon an instance of [[Alternative]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Alternative[F]): Alternative[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllAlternativeOps[F[_], A](target: F[A])(implicit tc: Alternative[F]): AllOps[F, A] {
      type TypeClassType = Alternative[F]
    } = new AllOps[F, A] {
      type TypeClassType = Alternative[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Alternative[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def unite[G[_], B](implicit ev$1: A <:< G[B], FM: Monad[F], G: Foldable[G]): F[B] =
      // Note: edited manually since seems Simulacrum is not able to handle the bin-compat redirection properly.
      typeClassInstance.unite[G, B](self.asInstanceOf[F[G[B]]])
    def separate[G[_, _], B, C](implicit ev$1: A <:< G[B, C], FM: Monad[F], G: Bifoldable[G]): (F[B], F[C]) =
      // Note: edited manually since seems Simulacrum is not able to handle the bin-compat redirection properly.
      typeClassInstance.separate[G, B, C](self.asInstanceOf[F[G[B, C]]])
    def separateFoldable[G[_, _], B, C](implicit ev$1: A <:< G[B, C], G: Bifoldable[G], FF: Foldable[F]): (F[B], F[C]) =
      typeClassInstance.separateFoldable[G, B, C](self.asInstanceOf[F[G[B, C]]])(G, FF)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with NonEmptyAlternative.AllOps[F, A] with MonoidK.AllOps[F, A] {
    type TypeClassType <: Alternative[F]
  }
  trait ToAlternativeOps extends Serializable {
    implicit def toAlternativeOps[F[_], A](target: F[A])(implicit tc: Alternative[F]): Ops[F, A] {
      type TypeClassType = Alternative[F]
    } = new Ops[F, A] {
      type TypeClassType = Alternative[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToAlternativeOps

}
