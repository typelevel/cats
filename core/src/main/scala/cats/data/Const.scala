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

import cats.kernel.{CommutativeMonoid, CommutativeSemigroup, LowerBounded, UpperBounded}

/**
 * [[Const]] is a phantom type, it does not contain a value of its second type parameter `B`
 * [[Const]] can be seen as a type level version of `Function.const[A, B]: A => B => A`
 */
final case class Const[A, B](getConst: A) {

  /**
   * changes the type of the second type parameter
   */
  def retag[C]: Const[A, C] =
    this.asInstanceOf[Const[A, C]]

  def combine(that: Const[A, B])(implicit A: Semigroup[A]): Const[A, B] =
    Const(A.combine(getConst, that.getConst))

  def traverse[F[_], C](f: B => F[C])(implicit F: Applicative[F]): F[Const[A, C]] =
    F.pure(retag[C])

  def ===(that: Const[A, B])(implicit A: Eq[A]): Boolean =
    A.eqv(getConst, that.getConst)

  def partialCompare(that: Const[A, B])(implicit A: PartialOrder[A]): Double =
    A.partialCompare(getConst, that.getConst)

  def compare(that: Const[A, B])(implicit A: Order[A]): Int =
    A.compare(getConst, that.getConst)

  def show(implicit A: Show[A]): String =
    s"Const(${A.show(getConst)})"
}

object Const extends ConstInstances {
  def empty[A, B](implicit A: Monoid[A]): Const[A, B] =
    Const(A.empty)

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class OfPartiallyApplied[B](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](a: A): Const[A, B] = Const(a)
  }

  /**
   * Convenient syntax for creating a Const[A, B] from an `A`
   * {{{
   * scala> import cats.data._
   * scala> Const.of[Int]("a")
   * res0: Const[String, Int] = Const(a)
   * }}}
   */
  def of[B]: OfPartiallyApplied[B] = new OfPartiallyApplied
}

sealed abstract private[data] class ConstInstances extends ConstInstances0 {
  implicit def catsDataUpperBoundedForConst[A, B](implicit A: UpperBounded[A]): UpperBounded[Const[A, B]] =
    new UpperBounded[Const[A, B]] {
      override def partialOrder: PartialOrder[Const[A, B]] = catsDataPartialOrderForConst(A.partialOrder)
      override def maxBound: Const[A, B] = Const(A.maxBound)
    }

  implicit def catsDataLowerBoundedForConst[A, B](implicit A: LowerBounded[A]): LowerBounded[Const[A, B]] =
    new LowerBounded[Const[A, B]] {
      override def partialOrder: PartialOrder[Const[A, B]] = catsDataPartialOrderForConst(A.partialOrder)
      override def minBound: Const[A, B] = Const(A.minBound)
    }

  implicit def catsDataOrderForConst[A: Order, B]: Order[Const[A, B]] = _ compare _

  implicit def catsDataAlignForConst[A: Semigroup]: Align[Const[A, *]] =
    new Align[Const[A, *]] {
      def align[B, C](fa: Const[A, B], fb: Const[A, C]): Const[A, Ior[B, C]] =
        Const(Semigroup[A].combine(fa.getConst, fb.getConst))
      def functor: Functor[Const[A, *]] = catsDataFunctorForConst
    }

  implicit def catsDataShowForConst[A: Show, B]: Show[Const[A, B]] = _.show

  implicit def catsDataTraverseForConst[C]: Traverse[Const[C, *]] =
    new Traverse[Const[C, *]] {
      def foldLeft[A, B](fa: Const[C, A], b: B)(f: (B, A) => B): B = b

      def foldRight[A, B](fa: Const[C, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

      override def size[A](fa: Const[C, A]): Long = 0L

      override def get[A](fa: Const[C, A])(idx: Long): Option[A] = None

      def traverse[G[_]: Applicative, A, B](fa: Const[C, A])(f: A => G[B]): G[Const[C, B]] =
        fa.traverse(f)

      override def mapAccumulate[S, A, B](init: S, fa: Const[C, A])(f: (S, A) => (S, B)): (S, Const[C, B]) =
        (init, fa.retag)
    }

  implicit def catsDataTraverseFilterForConst[C]: TraverseFilter[Const[C, *]] =
    new TraverseFilter[Const[C, *]] {

      override def mapFilter[A, B](fa: Const[C, A])(f: (A) => Option[B]): Const[C, B] = fa.retag

      override def collect[A, B](fa: Const[C, A])(f: PartialFunction[A, B]): Const[C, B] = fa.retag

      override def flattenOption[A](fa: Const[C, Option[A]]): Const[C, A] = fa.retag

      override def filter[A](fa: Const[C, A])(f: (A) => Boolean): Const[C, A] = fa.retag

      override def filterNot[A](fa: Const[C, A])(f: A => Boolean): Const[C, A] = fa.retag

      def traverseFilter[G[_], A, B](
        fa: Const[C, A]
      )(f: (A) => G[Option[B]])(implicit G: Applicative[G]): G[Const[C, B]] =
        G.pure(fa.retag[B])

      override def filterA[G[_], A](fa: Const[C, A])(f: (A) => G[Boolean])(implicit G: Applicative[G]): G[Const[C, A]] =
        G.pure(fa)

      val traverse: Traverse[Const[C, *]] = Const.catsDataTraverseForConst[C]
    }

  implicit def catsDataMonoidForConst[A: Monoid, B]: Monoid[Const[A, B]] =
    new Monoid[Const[A, B]] {
      def empty: Const[A, B] =
        Const.empty

      def combine(x: Const[A, B], y: Const[A, B]): Const[A, B] =
        x.combine(y)
    }

  implicit val catsDataBifoldableForConst: Bifoldable[Const] =
    new Bifoldable[Const] {
      def bifoldLeft[A, B, C](fab: Const[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        f(c, fab.getConst)

      def bifoldRight[A, B, C](fab: Const[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                             g: (B, Eval[C]) => Eval[C]
      ): Eval[C] =
        f(fab.getConst, c)
    }
}

sealed abstract private[data] class ConstInstances0 extends ConstInstances1 {

  implicit def catsDataContravariantMonoidalForConst[D: Monoid]: ContravariantMonoidal[Const[D, *]] =
    new ContravariantMonoidal[Const[D, *]] {
      override def unit = Const.empty[D, Unit]
      override def contramap[A, B](fa: Const[D, A])(f: B => A): Const[D, B] =
        fa.retag[B]
      override def product[A, B](fa: Const[D, A], fb: Const[D, B]): Const[D, (A, B)] =
        fa.retag[(A, B)].combine(fb.retag[(A, B)])
    }

  implicit def catsDataCommutativeApplicativeForConst[C](implicit
    C: CommutativeMonoid[C]
  ): CommutativeApplicative[Const[C, *]] =
    new ConstApplicative[C] with CommutativeApplicative[Const[C, *]] { val C0: CommutativeMonoid[C] = C }
}

sealed abstract private[data] class ConstInstances1 extends ConstInstances2 {

  implicit def catsDataCommutativeApplyForConst[C](implicit C: CommutativeSemigroup[C]): CommutativeApply[Const[C, *]] =
    new ConstApply[C] with CommutativeApply[Const[C, *]] { val C0: CommutativeSemigroup[C] = C }
}

sealed abstract private[data] class ConstInstances2 extends ConstInstances3 {

  implicit def catsDataSemigroupForConst[A: Semigroup, B]: Semigroup[Const[A, B]] = _ combine _

  implicit def catsDataPartialOrderForConst[A: PartialOrder, B]: PartialOrder[Const[A, B]] = _ partialCompare _

  implicit def catsDataApplicativeForConst[C](implicit C: Monoid[C]): Applicative[Const[C, *]] =
    new ConstApplicative[C] { val C0: Monoid[C] = C }
}

sealed abstract private[data] class ConstInstances3 extends ConstInstances4 {
  implicit def catsDataEqForConst[A: Eq, B]: Eq[Const[A, B]] = _ === _

  implicit def catsDataApplyForConst[C](implicit C: Semigroup[C]): Apply[Const[C, *]] =
    new ConstApply[C] { val C0: Semigroup[C] = C }
}

sealed abstract private[data] class ConstInstances4 {

  implicit def catsDataFunctorForConst[C]: Functor[Const[C, *]] =
    new ConstFunctor[C] {}

  implicit def catsDataContravariantForConst[C]: Contravariant[Const[C, *]] =
    new ConstContravariant[C] {}
}

sealed private[data] trait ConstFunctor[C] extends Functor[Const[C, *]] {
  def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] =
    fa.retag[B]
}

sealed private[data] trait ConstContravariant[C] extends Contravariant[Const[C, *]] {
  override def contramap[A, B](fa: Const[C, A])(f: B => A): Const[C, B] =
    fa.retag[B]
}

sealed private[data] trait ConstApply[C] extends ConstFunctor[C] with Apply[Const[C, *]] {

  implicit def C0: Semigroup[C]

  def ap[A, B](f: Const[C, A => B])(fa: Const[C, A]): Const[C, B] =
    f.retag[B].combine(fa.retag[B])

  override def product[A, B](fa: Const[C, A], fb: Const[C, B]): Const[C, (A, B)] =
    fa.retag[(A, B)].combine(fb.retag[(A, B)])
}

sealed private[data] trait ConstApplicative[C] extends ConstApply[C] with Applicative[Const[C, *]] {

  implicit def C0: Monoid[C]

  def pure[A](x: A): Const[C, A] =
    Const.empty
}
