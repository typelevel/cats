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

import cats.Contravariant

/**
 * [[Func]] is a function `A => F[B]`, where `F` is a functor.
 *
 * `Func` is similar to [[Kleisli]], but with weaker requirements. While `Kleisli` requires `FlatMap` (or `Monad`)
 * for sequential composition, `Func` only requires `Functor` and supports parallel (applicative) composition via
 * [[AppFunc]]. This makes `Func` useful when you want to compose effects in parallel rather than sequentially.
 *
 * '''Kleisli vs Func'''
 *
 * `Kleisli` is for ''sequential'' composition: `a => F[B]` andThen `b => F[C]` requires the output of the first
 * to feed into the second, and needs `FlatMap` to chain them. `Func` is for ''parallel'' composition: given
 * `a => F[B]` and `a => F[C]`, we can combine them into `a => F[(B, C)]` using only `Applicative#product`.
 *
 * Example:
 * {{{
 * scala> import cats.data._
 * scala> import cats.syntax.all._
 *
 * scala> val parseInt: Func[Option, String, Int] = Func.func(s => scala.util.Try(s.toInt).toOption)
 * scala> val double: Func[Option, String, Double] = Func.func(s => scala.util.Try(s.toDouble).toOption)
 *
 * scala> // Compose in parallel: parse and double the same input independently
 * scala> val combined: Func[Option, String, (Int, Double)] = parseInt.product(double)
 * scala> combined.run("42")
 * res0: Option[(Int, Double)] = Some((42,42.0))
 * }}}
 *
 * For more powerful composition (compose, andThen, traverse), see [[AppFunc]].
 *
 * See also: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 */
sealed abstract class Func[F[_], A, B] { self =>
  def run: A => F[B]

  /**
   * Lift a function `B => C` over this `Func`, producing a `Func[F, A, C]`.
   *
   * Example:
   * {{{
   * scala> import cats.data._
   * scala> import cats.syntax.all._
   *
   * scala> val f: Func[Option, Int, Int] = Func.func(i => Some(i + 1))
   * scala> f.map(_ * 10).run(5)
   * res0: Option[Int] = Some(60)
   * }}}
   */
  def map[C](f: B => C)(implicit FF: Functor[F]): Func[F, A, C] =
    Func.func(a => FF.map(self.run(a))(f))

  /**
   * Modify the context `F` using a natural transformation `f: F ~> G`.
   *
   * Example:
   * {{{
   * scala> import cats.data._
   * scala> import cats.arrow.FunctionK
   * scala> val f: Func[List, Int, Int] = Func.func(i => List(i + 1))
   * scala> val toOption: List ~> Option = FunctionK.from {
   *      |   case Nil => None
   *      |   case h :: _ => Some(h)
   *      | }
   * scala> f.mapK(toOption).run(5)
   * res0: Option[Int] = Some(6)
   * }}}
   */
  def mapK[G[_]](f: F ~> G): Func[G, A, B] =
    Func.func(a => f(run(a)))
}

object Func extends FuncInstances {

  /**
   * Create a `Func` from a function `A => F[B]`.
   *
   * Example:
   * {{{
   * scala> import cats.data._
   * scala> val f: Func[Option, String, Int] = Func.func(s => scala.util.Try(s.toInt).toOption)
   * scala> f.run("42")
   * res0: Option[Int] = Some(42)
   * }}}
   */
  def func[F[_], A, B](run0: A => F[B]): Func[F, A, B] =
    new Func[F, A, B] {
      def run: A => F[B] = run0
    }

  /**
   * Create an [[AppFunc]] (applicative function) from a function `A => F[B]`.
   *
   * `AppFunc` supports parallel composition via `product`, `compose`, `andThen`, and `traverse`.
   *
   * Example:
   * {{{
   * scala> import cats.data._
   * scala> import cats.syntax.all._
   *
   * scala> val parse: AppFunc[Option, String, Int] =
   *      |   Func.appFunc(s => scala.util.Try(s.toInt).toOption)
   * scala> val double: AppFunc[Option, String, Double] =
   *      |   Func.appFunc(s => scala.util.Try(s.toDouble * 2).toOption)
   *
   * scala> val combined = parse.product(double)
   * scala> combined.run("21")
   * res0: Option[(Int, Double)] = Some((21,42.0))
   * }}}
   */
  def appFunc[F[_], A, B](run0: A => F[B])(implicit FF: Applicative[F]): AppFunc[F, A, B] =
    new AppFunc[F, A, B] {
      def F: Applicative[F] = FF
      def run: A => F[B] = run0
    }

}

abstract private[data] class FuncInstances extends FuncInstances0 {
  implicit def catsDataApplicativeForFunc[F[_], C](implicit FF: Applicative[F]): Applicative[λ[α => Func[F, C, α]]] =
    new FuncApplicative[F, C] {
      def F: Applicative[F] = FF
    }
}

abstract private[data] class FuncInstances0 extends FuncInstances1 {
  implicit def catsDataApplyForFunc[F[_], C](implicit FF: Apply[F]): Apply[λ[α => Func[F, C, α]]] =
    new FuncApply[F, C] {
      def F: Apply[F] = FF
    }
}

abstract private[data] class FuncInstances1 {
  implicit def catsDataFunctorForFunc[F[_], C](implicit FF: Functor[F]): Functor[λ[α => Func[F, C, α]]] =
    new FuncFunctor[F, C] {
      def F: Functor[F] = FF
    }

  implicit def catsDataContravariantForFunc[F[_], C](implicit
    FC: Contravariant[F]
  ): Contravariant[λ[α => Func[F, α, C]]] =
    new FuncContravariant[F, C] {
      def F: Contravariant[F] = FC
    }
}

sealed private[data] trait FuncFunctor[F[_], C] extends Functor[λ[α => Func[F, C, α]]] {
  def F: Functor[F]
  override def map[A, B](fa: Func[F, C, A])(f: A => B): Func[F, C, B] =
    fa.map(f)(F)
}

sealed private[data] trait FuncContravariant[F[_], C] extends Contravariant[λ[α => Func[F, α, C]]] {
  def F: Contravariant[F]
  def contramap[A, B](fa: Func[F, A, C])(f: B => A): Func[F, B, C] =
    Func.func(a => fa.run(f(a)))
}

sealed private[data] trait FuncApply[F[_], C]
    extends Apply.AbstractApply[λ[α => Func[F, C, α]]]
    with FuncFunctor[F, C] {
  def F: Apply[F]
  def ap[A, B](f: Func[F, C, A => B])(fa: Func[F, C, A]): Func[F, C, B] =
    Func.func(c => F.ap(f.run(c))(fa.run(c)))
  override def product[A, B](fa: Func[F, C, A], fb: Func[F, C, B]): Func[F, C, (A, B)] =
    Func.func(c => F.product(fa.run(c), fb.run(c)))
}

sealed private[data] trait FuncApplicative[F[_], C] extends FuncApply[F, C] with Applicative[λ[α => Func[F, C, α]]] {
  def F: Applicative[F]
  def pure[A](a: A): Func[F, C, A] =
    Func.func(Function.const(F.pure(a)))
}

/**
 * An implementation of [[Func]] that's specialized to [[Applicative]].
 *
 * `AppFunc` is the more powerful version of [[Func]], requiring an `Applicative` instance for `F`.
 * It supports parallel composition via `product`, `compose`, `andThen`, and `traverse`.
 *
 * While [[Kleisli]] is for ''sequential'' composition (requires `FlatMap`/`Monad`), `AppFunc` is for
 * ''parallel'' composition (requires only `Applicative`). This means `AppFunc` can compose effects
 * that don't depend on each other, running them in parallel.
 *
 * Example:
 * {{{
 * scala> import cats.data._
 * scala> import cats.syntax.all._
 *
 * scala> val validateName: AppFunc[Either[String, *], Config, String] =
 *      |   Func.appFunc(c => if (c.name.nonEmpty) Right(c.name) else Left("Name required"))
 * scala> val validateAge: AppFunc[Either[String, *], Config, Int] =
 *      |   Func.appFunc(c => if (c.age > 0) Right(c.age) else Left("Invalid age"))
 *
 * scala> // Combine validators: both must pass
 * scala> val combined = validateName.product(validateAge)
 * }}}
 */
sealed abstract class AppFunc[F[_], A, B] extends Func[F, A, B] { self =>
  def F: Applicative[F]

  /**
   * Combine this `AppFunc` with another in parallel, producing a tuple of results.
   *
   * Example:
   * {{{
   * scala> import cats.data._
   * scala> import cats.syntax.all._
   *
   * scala> val f: AppFunc[Option, Int, Int] = Func.appFunc(i => Some(i + 1))
   * scala> val g: AppFunc[Option, Int, String] = Func.appFunc(i => Some(i.toString))
   * scala> f.product(g).run(42)
   * res0: Option[(Int, String)] = Some((43,"42"))
   * }}}
   */
  def product[G[_]](g: AppFunc[G, A, B]): AppFunc[λ[α => Tuple2K[F, G, α]], A, B] = {
    implicit val FF: Applicative[F] = self.F
    implicit val GG: Applicative[G] = g.F
    Func.appFunc[λ[α => Tuple2K[F, G, α]], A, B] { (a: A) =>
      Tuple2K(self.run(a), g.run(a))
    }
  }

  /**
   * Compose this `AppFunc` with another, where this function's input is the other's output.
   *
   * The resulting function's context is `Nested[G, F, *]`.
   *
   * Example:
   * {{{
   * scala> import cats.data._
   * scala> import cats.syntax.all._
   *
   * scala> val f: AppFunc[Option, Int, Int] = Func.appFunc(i => Some(i + 1))
   * scala> val g: AppFunc[Option, String, Int] = Func.appFunc(s => scala.util.Try(s.toInt).toOption)
   * scala> val composed = f.compose(g)
   * scala> composed.run("42")
   * res0: Nested[Option, Option, Int] = Nested(Some(Some(43)))
   * }}}
   */
  def compose[G[_], C](g: AppFunc[G, C, A]): AppFunc[Nested[G, F, *], C, B] = {
    implicit val gfApplicative: Applicative[Nested[G, F, *]] = Nested.catsDataApplicativeForNested[G, F](using g.F, F)
    Func.appFunc[Nested[G, F, *], C, B] { (c: C) =>
      Nested(g.F.map(g.run(c))(self.run))
    }
  }

  /**
   * Compose this `AppFunc` with another, where the other function's input is this function's output.
   *
   * This is the opposite direction of `compose`.
   */
  def andThen[G[_], C](g: AppFunc[G, B, C]): AppFunc[Nested[F, G, *], A, C] =
    g.compose(self)

  /**
   * Lift a function `B => C` over this `AppFunc`, producing an `AppFunc[F, A, C]`.
   */
  def map[C](f: B => C): AppFunc[F, A, C] = {
    implicit val FF: Applicative[F] = self.F
    Func.appFunc(a => F.map(self.run(a))(f))
  }

  /**
   * Apply this function to each element of a traversable structure.
   *
   * Example:
   * {{{
   * scala> import cats.data._
   * scala> import cats.syntax.all._
   *
   * scala> val f: AppFunc[Option, Int, Int] = Func.appFunc(i => Some(i * 2))
   * scala> f.traverse(List(1, 2, 3))
   * res0: Option[List[Int]] = Some(List(2, 4, 6))
   * }}}
   */
  def traverse[G[_]](ga: G[A])(implicit GG: Traverse[G]): F[G[B]] =
    GG.traverse(ga)(self.run)(using F)
}

object AppFunc extends AppFuncInstances

abstract private[data] class AppFuncInstances {
  implicit def appFuncApplicative[F[_], C](implicit FF: Applicative[F]): Applicative[λ[α => AppFunc[F, C, α]]] =
    new AppFuncApplicative[F, C] {
      def F: Applicative[F] = FF
    }
}

sealed abstract private[data] class AppFuncApplicative[F[_], C]
    extends Apply.AbstractApply[λ[α => AppFunc[F, C, α]]]
    with Applicative[λ[α => AppFunc[F, C, α]]] {

  def F: Applicative[F]
  override def map[A, B](fa: AppFunc[F, C, A])(f: A => B): AppFunc[F, C, B] =
    fa.map(f)
  def ap[A, B](f: AppFunc[F, C, A => B])(fa: AppFunc[F, C, A]): AppFunc[F, C, B] =
    Func.appFunc[F, C, B](c => F.ap(f.run(c))(fa.run(c)))(F)
  override def product[A, B](fa: AppFunc[F, C, A], fb: AppFunc[F, C, B]): AppFunc[F, C, (A, B)] =
    Func.appFunc[F, C, (A, B)](c => F.product(fa.run(c), fb.run(c)))(F)
  def pure[A](a: A): AppFunc[F, C, A] =
    Func.appFunc[F, C, A](Function.const(F.pure(a)))(F)
}
