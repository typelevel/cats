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
 * A type class of types which give rise to two independent, covariant
 * functors.
 */
trait Bifunctor[F[_, _]] extends Serializable { self =>

  /**
   * The quintessential method of the Bifunctor trait, it applies a
   * function to each "side" of the bifunctor.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val x: (List[String], Int) = (List("foo", "bar"), 3)
   * scala> x.bimap(_.headOption, _.toLong + 1)
   * res0: (Option[String], Long) = (Some(foo),4)
   * }}}
   */
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  def rightFunctor[X]: Functor[F[X, *]] =
    new RightFunctor[F, X] { val F = self }

  def leftFunctor[X]: Functor[F[*, X]] =
    new LeftFunctor[F, X] { val F = self }

  // derived methods
  /**
   * apply a function to the "left" functor
   */
  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, identity)

  /**
   * The composition of two Bifunctors is itself a Bifunctor
   */
  def compose[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifunctor[F, G] {
      val F = self
      val G = G0
    }

  /**
   * Widens A into a supertype AA.
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> sealed trait Foo
   * scala> case object Bar extends Foo
   * scala> val x1: Either[Bar.type, Int] = Either.left(Bar)
   * scala> val x2: Either[Foo, Int] = x1.leftWiden
   * }}}
   */
  def leftWiden[A, B, AA >: A](fab: F[A, B]): F[AA, B] = fab.asInstanceOf[F[AA, B]]

  /**
   * Lift left into F using Applicative.
   * * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val x0: Either[String, Int] = Either.left("foo")
   * scala> val x1: Either[List[String], Int] = x0.leftLiftTo[List]
   * }}}
   *
   */
  def leftLiftTo[A, B, C[_]](fab: F[A, B])(implicit C: Applicative[C]): F[C[A], B] =
    leftMap[A, B, C[A]](fab)(C.pure[A])

}

object Bifunctor extends cats.instances.NTupleBifunctorInstances {
  implicit def catsBifunctorForEither: Bifunctor[Either] = cats.instances.either.catsStdBitraverseForEither

  @deprecated("Use catsStdBitraverseForTuple2 in cats.instances.NTupleBitraverseInstances", "2.4.0")
  def catsBifunctorForTuple2: Bifunctor[Tuple2] = cats.instances.tuple.catsStdBitraverseForTuple2

  /**
   * Summon an instance of [[Bifunctor]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Bifunctor[F]): Bifunctor[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllBifunctorOps[F[_, _], A, B](target: F[A, B])(implicit tc: Bifunctor[F]): AllOps[F, A, B] {
      type TypeClassType = Bifunctor[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Bifunctor[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Bifunctor[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def bimap[C, D](f: A => C, g: B => D): F[C, D] = typeClassInstance.bimap[A, B, C, D](self)(f, g)
    def leftMap[C](f: A => C): F[C, B] = typeClassInstance.leftMap[A, B, C](self)(f)
    def leftWiden[C >: A]: F[C, B] = typeClassInstance.leftWiden[A, B, C](self)
    def leftLiftTo[C[_]](implicit C: Applicative[C]): F[C[A], B] =
      leftMap[C[A]](C.pure[A])

  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B]
  trait ToBifunctorOps extends Serializable {
    implicit def toBifunctorOps[F[_, _], A, B](target: F[A, B])(implicit tc: Bifunctor[F]): Ops[F, A, B] {
      type TypeClassType = Bifunctor[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Bifunctor[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToBifunctorOps

}

private[cats] trait ComposedBifunctor[F[_, _], G[_, _]] extends Bifunctor[λ[(A, B) => F[G[A, B], G[A, B]]]] {
  def F: Bifunctor[F]
  def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] = {
    val innerBimap: G[A, B] => G[C, D] = gab => G.bimap(gab)(f, g)
    F.bimap(fab)(innerBimap, innerBimap)
  }
}

abstract private class LeftFunctor[F[_, _], X] extends Functor[F[*, X]] {
  implicit val F: Bifunctor[F]

  override def map[A, C](fax: F[A, X])(f: A => C): F[C, X] =
    F.bimap(fax)(f, identity)
}

abstract private class RightFunctor[F[_, _], X] extends Functor[F[X, *]] {
  implicit val F: Bifunctor[F]

  override def map[A, C](fxa: F[X, A])(f: A => C): F[X, C] =
    F.bimap(fxa)(identity, f)
}
