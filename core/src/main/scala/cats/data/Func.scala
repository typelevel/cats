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
 * [[Func]] is a function `A => F[B]`.
 *
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 */
sealed abstract class Func[F[_], A, B] { self =>
  def run: A => F[B]
  def map[C](f: B => C)(implicit FF: Functor[F]): Func[F, A, C] =
    Func.func(a => FF.map(self.run(a))(f))

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): Func[G, A, B] =
    Func.func(a => f(run(a)))
}

object Func extends FuncInstances {

  /**
   * function `A => F[B]`.
   */
  def func[F[_], A, B](run0: A => F[B]): Func[F, A, B] =
    new Func[F, A, B] {
      def run: A => F[B] = run0
    }

  /**
   * applicative function.
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

sealed private[data] trait FuncApply[F[_], C] extends Apply[λ[α => Func[F, C, α]]] with FuncFunctor[F, C] {
  def F: Apply[F]
  def ap[A, B](f: Func[F, C, A => B])(fa: Func[F, C, A]): Func[F, C, B] =
    Func.func(c => F.ap(f.run(c))(fa.run(c)))
  override def product[A, B](fa: Func[F, C, A], fb: Func[F, C, B]): Func[F, C, (A, B)] =
    Func.func(c => F.product(fa.run(c), fb.run(c)))
}

sealed private[data] trait FuncApplicative[F[_], C] extends Applicative[λ[α => Func[F, C, α]]] with FuncApply[F, C] {
  def F: Applicative[F]
  def pure[A](a: A): Func[F, C, A] =
    Func.func(Function.const(F.pure(a)))
}

/**
 * An implementation of [[Func]] that's specialized to [[Applicative]].
 */
sealed abstract class AppFunc[F[_], A, B] extends Func[F, A, B] { self =>
  def F: Applicative[F]

  def product[G[_]](g: AppFunc[G, A, B]): AppFunc[λ[α => Tuple2K[F, G, α]], A, B] = {
    implicit val FF: Applicative[F] = self.F
    implicit val GG: Applicative[G] = g.F
    Func.appFunc[λ[α => Tuple2K[F, G, α]], A, B] { (a: A) =>
      Tuple2K(self.run(a), g.run(a))
    }
  }

  def compose[G[_], C](g: AppFunc[G, C, A]): AppFunc[Nested[G, F, *], C, B] = {
    implicit val gfApplicative: Applicative[Nested[G, F, *]] = Nested.catsDataApplicativeForNested[G, F](g.F, F)
    Func.appFunc[Nested[G, F, *], C, B] { (c: C) =>
      Nested(g.F.map(g.run(c))(self.run))
    }
  }

  def andThen[G[_], C](g: AppFunc[G, B, C]): AppFunc[Nested[F, G, *], A, C] =
    g.compose(self)

  def map[C](f: B => C): AppFunc[F, A, C] = {
    implicit val FF: Applicative[F] = self.F
    Func.appFunc(a => F.map(self.run(a))(f))
  }

  def traverse[G[_]](ga: G[A])(implicit GG: Traverse[G]): F[G[B]] =
    GG.traverse(ga)(self.run)(F)
}

object AppFunc extends AppFuncInstances

abstract private[data] class AppFuncInstances {
  implicit def appFuncApplicative[F[_], C](implicit FF: Applicative[F]): Applicative[λ[α => AppFunc[F, C, α]]] =
    new AppFuncApplicative[F, C] {
      def F: Applicative[F] = FF
    }
}

sealed private[data] trait AppFuncApplicative[F[_], C] extends Applicative[λ[α => AppFunc[F, C, α]]] {
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
