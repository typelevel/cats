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

import java.io.Serializable
import cats.arrow.{ArrowChoice, CommutativeArrow}
import scala.annotation.tailrec

/**
 * A function type of a single input that can do function composition
 * (via `andThen` and `compose`) in constant stack space with amortized
 * linear time application (in the number of constituent functions).
 *
 * Example:
 *
 * {{{
 *   val seed = AndThen((x: Int) => x + 1)
 *   val f = (0 until 10000).foldLeft(seed)((acc, _) => acc.andThen(_ + 1))
 *
 *   // This should not trigger stack overflow ;-)
 *   f(0)
 * }}}
 *
 * This can be used to build stack safe data structures that make
 * use of lambdas. The perfect candidates for usage with `AndThen`
 * are the data structures using a signature like this (where
 * `F[_]` is a monadic type):
 *
 * {{{
 *   A => F[B]
 * }}}
 *
 * As an example, if we described this data structure, the naive
 * solution for that `map` is stack unsafe:
 *
 * {{{
 *   case class Resource[F[_], A, B](
 *     acquire: F[A],
 *     use: A => F[B],
 *     release: A => F[Unit]) {
 *
 *     def flatMap[C](f: B => C)(implicit F: Functor[F]): Resource[F, A, C] = {
 *       Resource(
 *         ra.acquire,
 *         // Stack Unsafe!
 *         a => ra.use(a).map(f),
 *         ra.release)
 *     }
 *   }
 * }}}
 *
 * To describe a `flatMap` operation for this data type, `AndThen`
 * can save the day:
 *
 * {{{
 *   def flatMap[C](f: B => C)(implicit F: Functor[F]): Resource[F, A, C] = {
 *     Resource(
 *       ra.acquire,
 *       AndThen(ra.use).andThen(_.map(f)),
 *       ra.release)
 *   }
 * }}}
 */
sealed abstract class AndThen[-T, +R] extends (T => R) with Product with Serializable {

  import AndThen.*

  final def apply(a: T): R =
    runLoop(a)

  override def andThen[A](g: R => A): AndThen[T, A] =
    // Fusing calls up to a certain threshold, using the fusion
    // technique implemented for `cats.effect.IO#map`
    g match {
      case atg: AndThen[R, A] =>
        AndThen.andThen(this, atg)
      case _ =>
        this match {
          case Single(f, index) if index < fusionMaxStackDepth =>
            Single(f.andThen(g), index + 1)
          case Concat(left, Single(f, index)) if index < fusionMaxStackDepth =>
            Concat(left, Single(f.andThen(g), index + 1))
          case _ =>
            Concat(this, Single(g, 0))
        }
    }

  override def compose[A](g: A => T): AndThen[A, R] =
    // Fusing calls up to a certain threshold, using the fusion
    // technique implemented for `cats.effect.IO#map`
    g match {
      case atg: AndThen[A, T] => AndThen.andThen(atg, this)
      case _ =>
        this match {
          case Single(f, index) if index < fusionMaxStackDepth =>
            Single(f.compose(g), index + 1)
          case Concat(Single(f, index), right) if index < fusionMaxStackDepth =>
            Concat(Single(f.compose(g), index + 1), right)
          case _ =>
            Concat(Single(g, 0), this)
        }
    }

  private def runLoop(start: T): R = {
    @tailrec
    def loop[A](self: AndThen[A, R], current: A): R =
      self match {
        case Single(f, _) => f(current)

        case Concat(Single(f, _), right) =>
          loop(right, f(current))

        case Concat(left @ Concat(_, _), right) =>
          loop(left.rotateAccum(right), current)
      }

    loop(this, start)
  }

  // converts left-leaning to right-leaning
  final protected def rotateAccum[E](_right: AndThen[R, E]): AndThen[T, E] = {
    @tailrec
    def loop[A](left: AndThen[T, A], right: AndThen[A, E]): AndThen[T, E] =
      left match {
        case Concat(left1, right1) =>
          loop(left1, Concat(right1, right))
        case notConcat => Concat(notConcat, right)
      }

    loop(this, _right)
  }

  override def toString: String =
    "AndThen$" + System.identityHashCode(this)
}

object AndThen extends AndThenInstances0 {

  /**
   * Builds an [[AndThen]] reference by wrapping a plain function.
   */
  def apply[A, B](f: A => B): AndThen[A, B] =
    f match {
      case ref: AndThen[A, B] @unchecked => ref
      case _                             => Single(f, 0)
    }

  final private case class Single[-A, +B](f: A => B, index: Int) extends AndThen[A, B]
  final private case class Concat[-A, E, +B](left: AndThen[A, E], right: AndThen[E, B]) extends AndThen[A, B]

  /**
   * Establishes the maximum stack depth when fusing `andThen` or
   * `compose` calls.
   *
   * The default is `128`.
   *
   * This value was reached by taking into account the default stack
   * size as set on 32 bits or 64 bits, Linux or Windows systems,
   * being enough to notice performance gains, but not big enough
   * to be in danger of triggering a stack-overflow error.
   */
  final private val fusionMaxStackDepth = 128

  /**
   * If you are going to call this function many times, right associating it
   * once can be a significant performance improvement for VERY long chains.
   */
  def toRightAssociated[A, B](fn: AndThen[A, B]): AndThen[A, B] = {
    @tailrec
    def loop[X, Y](beg: AndThen[A, X], middle: AndThen[X, Y], end: AndThen[Y, B], endDone: Boolean): AndThen[A, B] =
      if (endDone) {
        // end is right associated
        middle match {
          case sm @ Single(_, _) =>
            // here we use andThen to fuse singles below
            // the threshold that may have been hidden
            // by Concat structure previously
            val newEnd = AndThen.andThen(sm, end)
            beg match {
              case sb @ Single(_, _) =>
                AndThen.andThen(sb, newEnd)
              case Concat(begA, begB) =>
                loop(begA, begB, newEnd, true)
            }
          case Concat(mA, mB) =>
            // rotate mA onto beg:
            // we don't need to use andThen here since we
            // are still preparing to put onto the end
            loop(Concat(beg, mA), mB, end, true)
        }
      } else {
        // we are still right-associating the end
        end match {
          case se @ Single(_, _)  => loop(beg, middle, se, true)
          case Concat(endA, endB) => loop(beg, Concat(middle, endA), endB, false)
        }
      }

    fn match {
      case Concat(Concat(a, b), c)                           => loop(a, b, c, false)
      case Concat(a, Concat(b, c))                           => loop(a, b, c, false)
      case Concat(Single(_, _), Single(_, _)) | Single(_, _) => fn
    }
  }

  /**
   * true if this fn is already right associated, which is the faster
   * for calling
   */
  @tailrec
  final def isRightAssociated[A, B](fn: AndThen[A, B]): Boolean =
    fn match {
      case Single(_, _)                => true
      case Concat(Single(_, _), right) => isRightAssociated(right)
      case _                           => false
    }

  final def andThen[A, B, C](ab: AndThen[A, B], bc: AndThen[B, C]): AndThen[A, C] =
    ab match {
      case Single(f, indexf) =>
        bc match {
          case Single(g, indexg) =>
            if (indexf + indexg < fusionMaxStackDepth) Single(f.andThen(g), indexf + indexg + 1)
            else Concat(ab, bc)

          case Concat(Single(g, indexg), right) if indexf + indexg < fusionMaxStackDepth =>
            Concat(Single(f.andThen(g), indexf + indexg + 1), right)

          case _ => Concat(ab, bc)
        }
      case Concat(leftf, Single(f, indexf)) =>
        bc match {
          case Single(g, indexg) =>
            if (indexf + indexg < fusionMaxStackDepth) Concat(leftf, Single(f.andThen(g), indexf + indexg + 1))
            else Concat(ab, bc)

          case Concat(Single(g, indexg), right) if indexf + indexg < fusionMaxStackDepth =>
            Concat(leftf, Concat(Single(f.andThen(g), indexf + indexg + 1), right))

          case _ =>
            Concat(ab, bc)
        }
      case _ => Concat(ab, bc)
    }
}

abstract private[data] class AndThenInstances0 extends AndThenInstances1 {

  /**
   * [[cats.Monad]] instance for [[AndThen]].
   */
  implicit def catsDataMonadForAndThen[T]: Monad[AndThen[T, *]] =
    new Monad[AndThen[T, *]] {
      // Piggybacking on the instance for Function1
      private[this] val fn1 = instances.all.catsStdMonadForFunction1[T]

      def pure[A](x: A): AndThen[T, A] =
        AndThen(fn1.pure[A](x))

      def flatMap[A, B](fa: AndThen[T, A])(f: A => AndThen[T, B]): AndThen[T, B] =
        AndThen(fn1.flatMap(fa)(f))

      override def map[A, B](fa: AndThen[T, A])(f: A => B): AndThen[T, B] =
        AndThen(f).compose(fa)

      def tailRecM[A, B](a: A)(f: A => AndThen[T, Either[A, B]]): AndThen[T, B] =
        AndThen(fn1.tailRecM(a)(f))
    }

  /**
   * [[cats.ContravariantMonoidal]] instance for [[AndThen]].
   */
  implicit def catsDataContravariantMonoidalForAndThen[R: Monoid]: ContravariantMonoidal[AndThen[*, R]] =
    new ContravariantMonoidal[AndThen[*, R]] {
      // Piggybacking on the instance for Function1
      private[this] val fn1 = instances.all.catsStdContravariantMonoidalForFunction1[R]

      def unit: AndThen[Unit, R] =
        AndThen(fn1.unit)

      def contramap[A, B](fa: AndThen[A, R])(f: B => A): AndThen[B, R] =
        fa.compose(f)

      def product[A, B](fa: AndThen[A, R], fb: AndThen[B, R]): AndThen[(A, B), R] =
        AndThen(fn1.product(fa, fb))
    }

  /**
   * [[cats.arrow.ArrowChoice ArrowChoice]] and
   * [[cats.arrow.CommutativeArrow CommutativeArrow]] instances
   * for [[AndThen]].
   */
  implicit val catsDataArrowForAndThen: ArrowChoice[AndThen] & CommutativeArrow[AndThen] =
    new ArrowChoice[AndThen] with CommutativeArrow[AndThen] {
      // Piggybacking on the instance for Function1
      private[this] val fn1 = instances.all.catsStdInstancesForFunction1

      def choose[A, B, C, D](f: AndThen[A, C])(g: AndThen[B, D]): AndThen[Either[A, B], Either[C, D]] =
        AndThen(fn1.choose(f)(g))

      def lift[A, B](f: A => B): AndThen[A, B] =
        AndThen(f)

      def first[A, B, C](fa: AndThen[A, B]): AndThen[(A, C), (B, C)] =
        AndThen(fn1.first(fa))

      override def split[A, B, C, D](f: AndThen[A, B], g: AndThen[C, D]): AndThen[(A, C), (B, D)] =
        AndThen(fn1.split(f, g))

      def compose[A, B, C](f: AndThen[B, C], g: AndThen[A, B]): AndThen[A, C] =
        AndThen.andThen(g, f)
    }
}

abstract private[data] class AndThenInstances1 {

  /**
   * [[cats.Contravariant]] instance for [[AndThen]].
   */
  implicit def catsDataContravariantForAndThen[R]: Contravariant[AndThen[*, R]] =
    new Contravariant[AndThen[*, R]] {
      def contramap[T1, T0](fa: AndThen[T1, R])(f: T0 => T1): AndThen[T0, R] =
        fa.compose(f)
    }
}
