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

import cats.arrow.Arrow
import cats.data.Chain

import scala.annotation.tailrec

/**
 * Applicative functor.
 *
 * Allows application of a function in an Applicative context to a value in an Applicative context
 *
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 * Also: [[http://staff.city.ac.uk/~ross/papers/Applicative.pdf Applicative programming with effects]]
 *
 * Must obey the laws defined in cats.laws.ApplicativeLaws.
 */
trait Applicative[F[_]] extends Apply[F] with InvariantMonoidal[F] { self =>

  /**
   * `pure` lifts any value into the Applicative Functor.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> Applicative[Option].pure(10)
   * res0: Option[Int] = Some(10)
   * }}}
   */
  def pure[A](x: A): F[A]

  /**
   * Returns an `F[Unit]` value, equivalent with `pure(())`.
   *
   * A useful shorthand, also allowing implementations to optimize the
   * returned reference (e.g. it can be a `val`).
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> Applicative[Option].unit
   * res0: Option[Unit] = Some(())
   * }}}
   */
  def unit: F[Unit] = pure(())

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(pure(f))(fa)

  /**
   * Given `fa` and `n`, apply `fa` `n` times to construct an `F[List[A]]` value.
   *
   * Example:
   * {{{
   * scala> import cats.data.State
   *
   * scala> type Counter[A] = State[Int, A]
   * scala> val getAndIncrement: Counter[Int] = State { i => (i + 1, i) }
   * scala> val getAndIncrement5: Counter[List[Int]] =
   *      | Applicative[Counter].replicateA(5, getAndIncrement)
   * scala> getAndIncrement5.run(0).value
   * res0: (Int, List[Int]) = (5,List(0, 1, 2, 3, 4))
   * }}}
   */
  def replicateA[A](n: Int, fa: F[A]): F[List[A]] =
    if (n <= 0) pure(Nil)
    else if (n == 1) {
      // if n == 1 don't incur the penalty two maps: .map(Chain.one(_)).map(_.toList)
      map(fa)(_ :: Nil)
    } else {
      val one = map(fa)(Chain.one(_))

      // invariant: n >= 1
      @tailrec def loop(fa: F[Chain[A]], n: Int, acc: F[Chain[A]]): F[Chain[A]] =
        if (n == 1) map2(fa, acc)(_.concat(_))
        else
          // n >= 2
          // so (n >> 1) >= 1 and we are allowed to call loop
          loop(
            map2(fa, fa)(_.concat(_)),
            n >> 1,
            if ((n & 1) == 1) map2(acc, fa)(_.concat(_)) else acc
          )

      map(loop(one, n - 1, one))(_.toList)
    }

  /**
   * Given `fa` and `n`, apply `fa` `n` times discarding results to return F[Unit].
   *
   * Example:
   * {{{
   * scala> import cats.data.State
   *
   * scala> type Counter[A] = State[Int, A]
   * scala> val getAndIncrement: Counter[Int] = State { i => (i + 1, i) }
   * scala> val getAndIncrement5: Counter[Unit] =
   *      | Applicative[Counter].replicateA_(5, getAndIncrement)
   * scala> getAndIncrement5.run(0).value
   * res0: (Int, Unit) = (5,())
   * }}}
   */
  def replicateA_[A](n: Int, fa: F[A]): F[Unit] =
    if (n <= 0) unit
    else if (n == 1) void(fa)
    else {
      val fvoid = void(fa)

      // invariant: n >= 1
      @tailrec def loop(fa: F[Unit], n: Int, acc: F[Unit]): F[Unit] =
        if (n == 1) productR(fa)(acc)
        else
          // n >= 2
          // so (n >> 1) >= 1 and we are allowed to call loop
          loop(
            productR(fa)(fa),
            n >> 1,
            if ((n & 1) == 1) productR(acc)(fa) else acc
          )

      loop(fvoid, n - 1, fvoid)
    }

  /**
   * Compose an `Applicative[F]` and an `Applicative[G]` into an
   * `Applicative[λ[α => F[G[α]]]]`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val alo = Applicative[List].compose[Option]
   *
   * scala> alo.pure(3)
   * res0: List[Option[Int]] = List(Some(3))
   *
   * scala> alo.product(List(None, Some(true), Some(false)), List(Some(2), None))
   * res1: List[Option[(Boolean, Int)]] = List(None, None, Some((true,2)), None, Some((false,2)), None)
   * }}}
   */
  def compose[G[_]: Applicative]: Applicative[λ[α => F[G[α]]]] =
    new ComposedApplicative[F, G] {
      val F = self
      val G = Applicative[G]
    }

  /**
   * Compose an `Applicative[F]` and a `ContravariantMonoidal[G]` into a
   * `ContravariantMonoidal[λ[α => F[G[α]]]]`.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.Comparison
   * scala> import cats.syntax.all._
   *
   * // compares strings by alphabetical order
   * scala> val alpha: Order[String] = Order[String]
   *
   * // compares strings by their length
   * scala> val strLength: Order[String] = Order.by[String, Int](_.length)
   *
   * scala> val stringOrders: List[Order[String]] = List(alpha, strLength)
   *
   * // first comparison is with alpha order, second is with string length
   * scala> stringOrders.map(o => o.comparison("abc", "de"))
   * res0: List[Comparison] = List(LessThan, GreaterThan)
   *
   * scala> val le = Applicative[List].composeContravariantMonoidal[Order]
   *
   * // create Int orders that convert ints to strings and then use the string orders
   * scala> val intOrders: List[Order[Int]] = le.contramap(stringOrders)(_.toString)
   *
   * // first comparison is with alpha order, second is with string length
   * scala> intOrders.map(o => o.comparison(12, 3))
   * res1: List[Comparison] = List(LessThan, GreaterThan)
   *
   * // create the `product` of the string order list and the int order list
   * // `p` contains a list of the following orders:
   * // 1. (alpha comparison on strings followed by alpha comparison on ints)
   * // 2. (alpha comparison on strings followed by length comparison on ints)
   * // 3. (length comparison on strings followed by alpha comparison on ints)
   * // 4. (length comparison on strings followed by length comparison on ints)
   * scala> val p: List[Order[(String, Int)]] = le.product(stringOrders, intOrders)
   *
   * scala> p.map(o => o.comparison(("abc", 12), ("def", 3)))
   * res2: List[Comparison] = List(LessThan, LessThan, LessThan, GreaterThan)
   * }}}
   */
  def composeContravariantMonoidal[G[_]: ContravariantMonoidal]: ContravariantMonoidal[λ[α => F[G[α]]]] =
    new ComposedApplicativeContravariantMonoidal[F, G] {
      val F = self
      val G = ContravariantMonoidal[G]
    }

  /**
   * Returns the given argument (mapped to Unit) if `cond` is `false`,
   * otherwise, unit lifted into F.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> Applicative[List].unlessA(true)(List(1, 2, 3))
   * res0: List[Unit] = List(())
   *
   * scala> Applicative[List].unlessA(false)(List(1, 2, 3))
   * res1: List[Unit] = List((), (), ())
   *
   * scala> Applicative[List].unlessA(true)(List.empty[Int])
   * res2: List[Unit] = List(())
   *
   * scala> Applicative[List].unlessA(false)(List.empty[Int])
   * res3: List[Unit] = List()
   * }}}
   */
  def unlessA[A](cond: Boolean)(f: => F[A]): F[Unit] =
    if (cond) unit else void(f)

  /**
   * Returns the given argument (mapped to Unit) if `cond` is `true`, otherwise,
   * unit lifted into F.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> Applicative[List].whenA(true)(List(1, 2, 3))
   * res0: List[Unit] = List((), (), ())
   *
   * scala> Applicative[List].whenA(false)(List(1, 2, 3))
   * res1: List[Unit] = List(())
   *
   * scala> Applicative[List].whenA(true)(List.empty[Int])
   * res2: List[Unit] = List()
   *
   * scala> Applicative[List].whenA(false)(List.empty[Int])
   * res3: List[Unit] = List(())
   * }}}
   */
  def whenA[A](cond: Boolean)(f: => F[A]): F[Unit] =
    if (cond) void(f) else unit

}

object Applicative {
  def monoid[F[_], A](implicit f: Applicative[F], monoid: Monoid[A]): Monoid[F[A]] =
    new ApplicativeMonoid[F, A](f, monoid)

  /**
   * Creates an applicative functor for `F`, holding domain fixed and combining
   * over the codomain.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.Applicative.catsApplicativeForArrow
   * scala> val toLong: Int => Long = _.toLong
   * scala> val double: Int => Int = 2*_
   * scala> val f: Int => (Long, Int) = catsApplicativeForArrow[Function1, Int].product(toLong, double)
   * scala> f(3)
   * res0: (Long, Int) = (3,6)
   * }}}
   */
  def catsApplicativeForArrow[F[_, _], A](implicit F: Arrow[F]): Applicative[F[A, *]] =
    new ArrowApplicative[F, A](F)

  /**
   * Creates a CoflatMap for an Applicative `F`.
   * Cannot be implicit in 1.0 for Binary Compatibility Reasons
   *
   * Example:
   * {{{
   * scala> import cats._
   * scala> import cats.syntax.all._
   * scala> val fa = Some(3)
   * fa: Option[Int] = Some(3)
   * scala> Applicative.coflatMap[Option].coflatten(fa)
   * res0: Option[Option[Int]] = Some(Some(3))
   * }}}
   */
  def coflatMap[F[_]](implicit F: Applicative[F]): CoflatMap[F] =
    new CoflatMap[F] {
      def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B] = F.pure(f(fa))
      def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
    }

  /**
   * Summon an instance of [[Applicative]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Applicative[F]): Applicative[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllApplicativeOps[F[_], A](target: F[A])(implicit tc: Applicative[F]): AllOps[F, A] {
      type TypeClassType = Applicative[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Applicative[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Applicative[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Apply.AllOps[F, A] with InvariantMonoidal.AllOps[F, A] {
    type TypeClassType <: Applicative[F]
  }
  trait ToApplicativeOps extends Serializable {
    implicit def toApplicativeOps[F[_], A](target: F[A])(implicit tc: Applicative[F]): Ops[F, A] {
      type TypeClassType = Applicative[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Applicative[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToApplicativeOps

}

private[cats] class ApplicativeMonoid[F[_], A](f: Applicative[F], monoid: Monoid[A])
    extends ApplySemigroup(f, monoid)
    with Monoid[F[A]] {
  def empty: F[A] = f.pure(monoid.empty)
}

private[cats] class ArrowApplicative[F[_, _], A](F: Arrow[F]) extends Applicative[F[A, *]] {
  def pure[B](b: B): F[A, B] = F.lift[A, B](_ => b)
  override def map[B, C](fb: F[A, B])(f: B => C): F[A, C] = F.rmap(fb)(f)
  def ap[B, C](ff: F[A, B => C])(fb: F[A, B]): F[A, C] =
    F.rmap(F.andThen(F.lift((x: A) => (x, x)), F.split(ff, fb)))(tup => tup._1(tup._2))
  override def product[B, C](fb: F[A, B], fc: F[A, C]): F[A, (B, C)] =
    F.andThen(F.lift((x: A) => (x, x)), F.split(fb, fc))
}
