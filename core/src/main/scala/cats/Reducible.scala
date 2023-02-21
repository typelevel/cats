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

import cats.Foldable.Source
import cats.data.{Ior, NonEmptyList}

/**
 * Data structures that can be reduced to a summary value.
 *
 * `Reducible` is like a non-empty `Foldable`. In addition to the fold
 * methods it provides reduce methods which do not require an initial
 * value.
 *
 * In addition to the methods needed by `Foldable`, `Reducible` is
 * implemented in terms of two methods:
 *
 *  - `reduceLeftTo(fa)(f)(g)` eagerly reduces with an additional mapping function
 *  - `reduceRightTo(fa)(f)(g)` lazily reduces with an additional mapping function
 */
trait Reducible[F[_]] extends Foldable[F] { self =>

  /**
   * Left-associative reduction on `F` using the function `f`.
   *
   * Implementations should override this method when possible.
   */
  def reduceLeft[A](fa: F[A])(f: (A, A) => A): A =
    reduceLeftTo(fa)(identity)(f)

  /**
   * Right-associative reduction on `F` using the function `f`.
   */
  def reduceRight[A](fa: F[A])(f: (A, Eval[A]) => Eval[A]): Eval[A] =
    reduceRightTo(fa)(identity)(f)

  /**
   * Reduce a `F[A]` value using the given `Semigroup[A]`.
   */
  def reduce[A](fa: F[A])(implicit A: Semigroup[A]): A =
    reduceLeft(fa)(A.combine)

  /**
   * Reduce a `F[G[A]]` value using `SemigroupK[G]`, a universal
   * semigroup for `G[_]`.
   *
   * This method is a generalization of `reduce`.
   *
   * {{{
   * scala> import cats.Reducible
   * scala> import cats.data._
   * scala> Reducible[NonEmptyVector].reduceK(NonEmptyVector.of(NonEmptyList.of(1, 2, 3), NonEmptyList.of(4, 5, 6), NonEmptyList.of(7, 8, 9)))
   * res0: NonEmptyList[Int] = NonEmptyList(1, 2, 3, 4, 5, 6, 7, 8, 9)
   * }}}
   */
  def reduceK[G[_], A](fga: F[G[A]])(implicit G: SemigroupK[G]): G[A] =
    reduce(fga)(G.algebra)

  /**
   * Apply `f` to each element of `fa` and combine them using the
   * given `Semigroup[B]`.
   *
   * {{{
   * scala> import cats.Reducible
   * scala> import cats.data.NonEmptyList
   * scala> Reducible[NonEmptyList].reduceMap(NonEmptyList.of(1, 2, 3))(v => v.toString * v)
   * res0: String = 122333
   *
   * scala> val gt5: Int => Option[Int] = (num: Int) => Some(num).filter(_ > 5)
   * scala> Reducible[NonEmptyList].reduceMap(NonEmptyList.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(gt5)
   * res1: Option[Int] = Some(40)
   * }}}
   */
  def reduceMap[A, B](fa: F[A])(f: A => B)(implicit B: Semigroup[B]): B =
    reduceLeftTo(fa)(f)((b, a) => B.combine(b, f(a)))

  /**
   * Apply `f` to each element of `fa` and combine them using the
   * given `SemigroupK[G]`.
   *
   * {{{
   * scala> import cats._, cats.data._
   * scala> val f: Int => Endo[String] = i => (s => s + i)
   * scala> val x: Endo[String] = Reducible[NonEmptyList].reduceMapK(NonEmptyList.of(1, 2, 3))(f)
   * scala> val a = x("foo")
   * a: String = "foo321"
   * }}}
   */

  def reduceMapK[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: SemigroupK[G]): G[B] =
    reduceLeftTo(fa)(f)((b, a) => G.combineK(b, f(a)))

  /**
   * Apply `f` to the "initial element" of `fa` and combine it with
   * every other value using the given function `g`.
   */
  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B

  /**
   * Monadic variant of [[reduceLeftTo]].
   */
  def reduceLeftM[G[_], A, B](fa: F[A])(f: A => G[B])(g: (B, A) => G[B])(implicit G: FlatMap[G]): G[B] =
    reduceLeftTo(fa)(f)((gb, a) => G.flatMap(gb)(g(_, a)))

  /**
   * Reduce a `F[G[A]]` value using `Applicative[G]` and `Semigroup[A]`, a universal
   * semigroup for `G[_]`.
   *
   * This method is similar to [[reduce]], but may short-circuit.
   */
  def reduceA[G[_], A](fga: F[G[A]])(implicit G: Apply[G], A: Semigroup[A]): G[A] =
    reduceMapA(fga)(identity)

  /**
   * Reduce in an [[Apply]] context by mapping the `A` values to `G[B]`. combining
   * the `B` values using the given `Semigroup[B]` instance.
   *
   * Similar to [[reduceMapM]], but may be less efficient.
   *
   * {{{
   * scala> import cats.Reducible
   * scala> import cats.data.NonEmptyList
   * scala> val evenOpt: Int => Option[Int] =
   *      |   i => if (i % 2 == 0) Some(i) else None
   * scala> val allEven = NonEmptyList.of(2,4,6,8,10)
   * allEven: cats.data.NonEmptyList[Int] = NonEmptyList(2, 4, 6, 8, 10)
   * scala> val notAllEven = allEven ++ List(11)
   * notAllEven: cats.data.NonEmptyList[Int] = NonEmptyList(2, 4, 6, 8, 10, 11)
   * scala> Reducible[NonEmptyList].reduceMapA(allEven)(evenOpt)
   * res0: Option[Int] = Some(30)
   * scala> Reducible[NonEmptyList].reduceMapA(notAllEven)(evenOpt)
   * res1: Option[Int] = None
   * }}}
   */
  def reduceMapA[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Apply[G], B: Semigroup[B]): G[B] =
    reduceRightTo(fa)(f)((a, egb) => G.map2Eval(f(a), egb)(B.combine)).value

  /**
   * Reduce in an [[FlatMap]] context by mapping the `A` values to `G[B]`. combining
   * the `B` values using the given `Semigroup[B]` instance.
   *
   * Similar to [[reduceLeftM]], but using a `Semigroup[B]`. May be more efficient than [[reduceMapA]].
   *
   * {{{
   * scala> import cats.Reducible
   * scala> import cats.data.NonEmptyList
   * scala> val evenOpt: Int => Option[Int] =
   *      |   i => if (i % 2 == 0) Some(i) else None
   * scala> val allEven = NonEmptyList.of(2,4,6,8,10)
   * allEven: cats.data.NonEmptyList[Int] = NonEmptyList(2, 4, 6, 8, 10)
   * scala> val notAllEven = allEven ++ List(11)
   * notAllEven: cats.data.NonEmptyList[Int] = NonEmptyList(2, 4, 6, 8, 10, 11)
   * scala> Reducible[NonEmptyList].reduceMapM(allEven)(evenOpt)
   * res0: Option[Int] = Some(30)
   * scala> Reducible[NonEmptyList].reduceMapM(notAllEven)(evenOpt)
   * res1: Option[Int] = None
   * }}}
   */
  def reduceMapM[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: FlatMap[G], B: Semigroup[B]): G[B] =
    reduceRightTo(fa)(f)((a, egb) => G.map2Eval(f(a), egb)(B.combine)).value

  /**
   * Overridden from [[Foldable]] for efficiency.
   */
  override def reduceLeftToOption[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): Option[B] =
    Some(reduceLeftTo(fa)(f)(g))

  /**
   * Apply `f` to the "initial element" of `fa` and lazily combine it
   * with every other value using the given function `g`.
   */
  def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B]

  /**
   * Overridden from [[Foldable]] for efficiency.
   */
  override def reduceRightToOption[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
    reduceRightTo(fa)(f)(g).map(Some(_))

  /**
   * Traverse `F[A]` using `Apply[G]`.
   *
   * `A` values will be mapped into `G[B]` and combined using
   * `Apply#map2`.
   *
   * This method is similar to [[Foldable.traverse_]]. There are two
   * main differences:
   *
   * 1. We only need an [[Apply]] instance for `G` here, since we
   * don't need to call [[Applicative.pure]] for a starting value.
   * 2. This performs a strict left-associative traversal and thus
   * must always traverse the entire data structure. Prefer
   * [[Foldable.traverse_]] if you have an [[Applicative]] instance
   * available for `G` and want to take advantage of short-circuiting
   * the traversal.
   */
  def nonEmptyTraverse_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Apply[G]): G[Unit] = {
    val f1 = f.andThen(G.void)
    reduceRightTo(fa)(f1)((x, y) => G.map2Eval(f1(x), y)((_, b) => b)).value
  }

  /**
   * Sequence `F[G[A]]` using `Apply[G]`.
   *
   * This method is similar to [[Foldable.sequence_]] but requires only
   * an [[Apply]] instance for `G` instead of [[Applicative]]. See the
   * [[nonEmptyTraverse_]] documentation for a description of the differences.
   */
  def nonEmptySequence_[G[_], A](fga: F[G[A]])(implicit G: Apply[G]): G[Unit] =
    nonEmptyTraverse_(fga)(identity)

  def toNonEmptyList[A](fa: F[A]): NonEmptyList[A] =
    reduceRightTo(fa)(a => NonEmptyList(a, Nil)) { (a, lnel) =>
      lnel.map { case NonEmptyList(h, t) => NonEmptyList(a, h :: t) }
    }.value

  def compose[G[_]: Reducible]: Reducible[λ[α => F[G[α]]]] =
    new ComposedReducible[F, G] {
      val F = self
      val G = Reducible[G]
    }

  def minimum[A](fa: F[A])(implicit A: Order[A]): A =
    reduceLeft(fa)(A.min)

  def maximum[A](fa: F[A])(implicit A: Order[A]): A =
    reduceLeft(fa)(A.max)

  /**
   * Find the minimum `A` item in this structure according to an `Order.by(f)`.
   *
   * @see [[maximumBy]] for maximum instead of minimum.
   */
  def minimumBy[A, B: Order](fa: F[A])(f: A => B): A =
    minimum(fa)(Order.by(f))

  /**
   * Find the maximum `A` item in this structure according to an `Order.by(f)`.
   *
   * @see [[minimumBy]] for minimum instead of maximum.
   */
  def maximumBy[A, B: Order](fa: F[A])(f: A => B): A =
    maximum(fa)(Order.by(f))

  /**
   * Find all the minimum `A` items in this structure.
   * For all elements in the result Order.eqv(x, y) is true. Preserves order.
   *
   * @see [[maximumNel]] for maximum instead of minimum.
   */
  def minimumNel[A](fa: F[A])(implicit A: Order[A]): NonEmptyList[A] =
    reduceLeftTo(fa)(NonEmptyList.one) {
      case (l @ NonEmptyList(b, _), a) if A.compare(a, b) > 0  => l
      case (l @ NonEmptyList(b, _), a) if A.compare(a, b) == 0 => a :: l
      case (_, a)                                              => NonEmptyList.one(a)
    }.reverse

  /**
   * Find all the maximum `A` items in this structure.
   * For all elements in the result Order.eqv(x, y) is true. Preserves order.
   *
   * @see [[minimumNel]] for minimum instead of maximum.
   */
  def maximumNel[A](fa: F[A])(implicit A: Order[A]): NonEmptyList[A] =
    reduceLeftTo(fa)(NonEmptyList.one) {
      case (l @ NonEmptyList(b, _), a) if A.compare(a, b) < 0  => l
      case (l @ NonEmptyList(b, _), a) if A.compare(a, b) == 0 => a :: l
      case (_, a)                                              => NonEmptyList.one(a)
    }.reverse

  /**
   * Find all the minimum `A` items in this structure according to an `Order.by(f)`.
   * For all elements in the result Order.eqv(x, y) is true. Preserves order.
   *
   * @see [[maximumByNel]] for maximum instead of minimum.
   */
  def minimumByNel[A, B: Order](fa: F[A])(f: A => B): NonEmptyList[A] =
    minimumNel(fa)(Order.by(f))

  /**
   * Find all the maximum `A` items in this structure according to an `Order.by(f)`.
   * For all elements in the result Order.eqv(x, y) is true. Preserves order.
   *
   * @see [[minimumByNel]] for minimum instead of maximum.
   */
  def maximumByNel[A, B: Order](fa: F[A])(f: A => B): NonEmptyList[A] =
    maximumNel(fa)(Order.by(f))

  /**
   * Intercalate/insert an element between the existing elements while reducing.
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of("a", "b", "c")
   * scala> Reducible[NonEmptyList].nonEmptyIntercalate(nel, "-")
   * res0: String = a-b-c
   * scala> Reducible[NonEmptyList].nonEmptyIntercalate(NonEmptyList.of("a"), "-")
   * res1: String = a
   * }}}
   */
  def nonEmptyIntercalate[A](fa: F[A], a: A)(implicit A: Semigroup[A]): A =
    reduce(fa)(A.intercalate(a))

  /**
   * Partition this Reducible by a separating function `A => Either[B, C]`
   *
   * {{{
   * scala> import cats.data.NonEmptyList
   * scala> val nel = NonEmptyList.of(1,2,3,4)
   * scala> Reducible[NonEmptyList].nonEmptyPartition(nel)(a => if (a % 2 == 0) Left(a.toString) else Right(a))
   * res0: cats.data.Ior[cats.data.NonEmptyList[String],cats.data.NonEmptyList[Int]] = Both(NonEmptyList(2, 4),NonEmptyList(1, 3))
   * scala> Reducible[NonEmptyList].nonEmptyPartition(nel)(a => Right(a * 4))
   * res1: cats.data.Ior[cats.data.NonEmptyList[Nothing],cats.data.NonEmptyList[Int]] = Right(NonEmptyList(4, 8, 12, 16))
   * }}}
   */
  def nonEmptyPartition[A, B, C](fa: F[A])(f: A => Either[B, C]): Ior[NonEmptyList[B], NonEmptyList[C]] = {
    def g(a: A, eval: Eval[Ior[NonEmptyList[B], NonEmptyList[C]]]): Eval[Ior[NonEmptyList[B], NonEmptyList[C]]] =
      eval.map(ior =>
        (f(a), ior) match {
          case (Right(c), Ior.Left(_)) => ior.putRight(NonEmptyList.one(c))
          case (Right(c), _)           => ior.map(c :: _)
          case (Left(b), Ior.Right(r)) => Ior.bothNel(b, r)
          case (Left(b), _)            => ior.leftMap(b :: _)
        }
      )

    reduceRightTo(fa)(a =>
      f(a) match {
        case Right(c) => Ior.right(NonEmptyList.one(c))
        case Left(b)  => Ior.left(NonEmptyList.one(b))
      }
    )(g).value
  }

  override def isEmpty[A](fa: F[A]): Boolean = false

  override def nonEmpty[A](fa: F[A]): Boolean = true

  override def minimumOption[A](fa: F[A])(implicit A: Order[A]): Option[A] =
    Some(minimum(fa))

  override def maximumOption[A](fa: F[A])(implicit A: Order[A]): Option[A] =
    Some(maximum(fa))
}

object Reducible {

  /**
   * Summon an instance of [[Reducible]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Reducible[F]): Reducible[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllReducibleOps[F[_], A](target: F[A])(implicit tc: Reducible[F]): AllOps[F, A] {
      type TypeClassType = Reducible[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Reducible[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Reducible[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def reduceLeft(f: (A, A) => A): A = typeClassInstance.reduceLeft[A](self)(f)
    def reduceRight(f: (A, Eval[A]) => Eval[A]): Eval[A] = typeClassInstance.reduceRight[A](self)(f)
    def reduce(implicit A: Semigroup[A]): A = typeClassInstance.reduce[A](self)(A)
    def reduceK[G[_], B](implicit ev$1: A <:< G[B], G: SemigroupK[G]): G[B] =
      typeClassInstance.reduceK[G, B](self.asInstanceOf[F[G[B]]])(G)
    def reduceMap[B](f: A => B)(implicit B: Semigroup[B]): B = typeClassInstance.reduceMap[A, B](self)(f)(B)
    def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B = typeClassInstance.reduceLeftTo[A, B](self)(f)(g)
    def reduceLeftM[G[_], B](f: A => G[B])(g: (B, A) => G[B])(implicit G: FlatMap[G]): G[B] =
      typeClassInstance.reduceLeftM[G, A, B](self)(f)(g)(G)
    def reduceMapA[G[_], B](f: A => G[B])(implicit G: Apply[G], B: Semigroup[B]): G[B] =
      typeClassInstance.reduceMapA[G, A, B](self)(f)(G, B)
    def reduceMapM[G[_], B](f: A => G[B])(implicit G: FlatMap[G], B: Semigroup[B]): G[B] =
      typeClassInstance.reduceMapM[G, A, B](self)(f)(G, B)
    def reduceRightTo[B](f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      typeClassInstance.reduceRightTo[A, B](self)(f)(g)
    def nonEmptyTraverse_[G[_], B](f: A => G[B])(implicit G: Apply[G]): G[Unit] =
      typeClassInstance.nonEmptyTraverse_[G, A, B](self)(f)(G)
    def nonEmptySequence_[G[_], B](implicit ev$1: A <:< G[B], G: Apply[G]): G[Unit] =
      typeClassInstance.nonEmptySequence_[G, B](self.asInstanceOf[F[G[B]]])(G)
    def toNonEmptyList: NonEmptyList[A] = typeClassInstance.toNonEmptyList[A](self)
    def minimum(implicit A: Order[A]): A = typeClassInstance.minimum[A](self)(A)
    def maximum(implicit A: Order[A]): A = typeClassInstance.maximum[A](self)(A)
    def minimumBy[B](f: A => B)(implicit ev$1: Order[B]): A = typeClassInstance.minimumBy[A, B](self)(f)
    def maximumBy[B](f: A => B)(implicit ev$1: Order[B]): A = typeClassInstance.maximumBy[A, B](self)(f)
    def minimumNel(implicit A: Order[A]): NonEmptyList[A] = typeClassInstance.minimumNel[A](self)(A)
    def maximumNel(implicit A: Order[A]): NonEmptyList[A] = typeClassInstance.maximumNel[A](self)(A)
    def minimumByNel[B](f: A => B)(implicit ev$1: Order[B]): NonEmptyList[A] =
      typeClassInstance.minimumByNel[A, B](self)(f)
    def maximumByNel[B](f: A => B)(implicit ev$1: Order[B]): NonEmptyList[A] =
      typeClassInstance.maximumByNel[A, B](self)(f)
    def nonEmptyIntercalate(a: A)(implicit A: Semigroup[A]): A = typeClassInstance.nonEmptyIntercalate[A](self, a)(A)
    def nonEmptyPartition[B, C](f: A => Either[B, C]): Ior[NonEmptyList[B], NonEmptyList[C]] =
      typeClassInstance.nonEmptyPartition[A, B, C](self)(f)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Foldable.AllOps[F, A] {
    type TypeClassType <: Reducible[F]
  }
  trait ToReducibleOps extends Serializable {
    implicit def toReducibleOps[F[_], A](target: F[A])(implicit tc: Reducible[F]): Ops[F, A] {
      type TypeClassType = Reducible[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Reducible[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToReducibleOps

}
