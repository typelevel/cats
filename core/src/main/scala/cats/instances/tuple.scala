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
package instances

import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}

import scala.annotation.tailrec

trait TupleInstances extends Tuple2Instances with cats.kernel.instances.TupleInstances

private[instances] trait Tuple2InstancesBinCompat0 {

  /**
   * Witness for: (A, A) <-> Boolean => A
   */
  implicit def catsDataRepresentableForPair(implicit
    PF: Functor[λ[P => (P, P)]]
  ): Representable.Aux[λ[P => (P, P)], Boolean] =
    new Representable[λ[P => (P, P)]] {
      override type Representation = Boolean
      override val F: Functor[λ[P => (P, P)]] = PF

      override def tabulate[A](f: Boolean => A): (A, A) = (f(true), f(false))

      override def index[A](pair: (A, A)): Boolean => A = {
        case true  => pair._1
        case false => pair._2
      }
    }

  implicit val catsDataFunctorForPair: Functor[λ[P => (P, P)]] = new Functor[λ[P => (P, P)]] {
    override def map[A, B](fa: (A, A))(f: A => B): (B, B) = (f(fa._1), f(fa._2))
  }
}

sealed private[instances] trait Tuple2Instances extends Tuple2Instances1 {
  @deprecated("Use catsStdBitraverseForTuple2 in cats.instances.NTupleBitraverseInstances", "2.4.0")
  val catsStdBitraverseForTuple2: Bitraverse[Tuple2] =
    new Bitraverse[Tuple2] {
      def bitraverse[G[_]: Applicative, A, B, C, D](fab: (A, B))(f: A => G[C], g: B => G[D]): G[(C, D)] =
        Applicative[G].tuple2(f(fab._1), g(fab._2))

      def bifoldLeft[A, B, C](fab: (A, B), c: C)(f: (C, A) => C, g: (C, B) => C): C =
        g(f(c, fab._1), fab._2)

      def bifoldRight[A, B, C](fab: (A, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                        g: (B, Eval[C]) => Eval[C]
      ): Eval[C] =
        g(fab._2, f(fab._1, c))
    }

  @deprecated("Use catsStdShowForTuple2 in cats.instances.NTupleShowInstances", "2.4.0")
  def catsStdShowForTuple2[A, B](implicit aShow: Show[A], bShow: Show[B]): Show[(A, B)] = { case (a, b) =>
    s"(${aShow.show(a)},${bShow.show(b)})"
  }

  @deprecated("Use catsStdInstancesForTuple2 in cats.instances.NTupleMonadInstances", "2.4.0")
  def catsStdInstancesForTuple2[X]: Traverse[(X, *)] & Comonad[(X, *)] & Reducible[(X, *)] =
    new Traverse[(X, *)] with Comonad[(X, *)] with Reducible[(X, *)] {
      def traverse[G[_], A, B](fa: (X, A))(f: A => G[B])(implicit G: Applicative[G]): G[(X, B)] =
        G.map(f(fa._2))((fa._1, _))

      def foldLeft[A, B](fa: (X, A), b: B)(f: (B, A) => B): B = f(b, fa._2)

      def foldRight[A, B](fa: (X, A), lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa._2, lb)

      override def map[A, B](fa: (X, A))(f: A => B): (X, B) = (fa._1, f(fa._2))

      override def mapAccumulate[S, A, B](init: S, fa: (X, A))(f: (S, A) => (S, B)): (S, (X, B)) = {
        val (snext, b) = f(init, fa._2)
        (snext, (fa._1, b))
      }

      def coflatMap[A, B](fa: (X, A))(f: ((X, A)) => B): (X, B) = (fa._1, f(fa))

      def extract[A](fa: (X, A)): A = fa._2

      override def coflatten[A](fa: (X, A)): (X, (X, A)) = (fa._1, fa)

      override def foldMap[A, B](fa: (X, A))(f: A => B)(implicit B: Monoid[B]): B = f(fa._2)

      override def reduce[A](fa: (X, A))(implicit A: Semigroup[A]): A = fa._2

      def reduceLeftTo[A, B](fa: (X, A))(f: A => B)(g: (B, A) => B): B = f(fa._2)

      override def reduceLeft[A](fa: (X, A))(f: (A, A) => A): A = fa._2

      override def reduceLeftToOption[A, B](fa: (X, A))(f: A => B)(g: (B, A) => B): Option[B] =
        Some(f(fa._2))

      override def reduceRight[A](fa: (X, A))(f: (A, Eval[A]) => Eval[A]): Eval[A] =
        Now(fa._2)

      def reduceRightTo[A, B](fa: (X, A))(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        Now(f(fa._2))

      override def reduceRightToOption[A, B](fa: (X, A))(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
        Now(Some(f(fa._2)))

      override def reduceMap[A, B](fa: (X, A))(f: A => B)(implicit B: Semigroup[B]): B =
        f(fa._2)

      override def size[A](fa: (X, A)): Long = 1L

      override def toList[A](fa: (X, A)): List[A] = fa._2 :: Nil

      override def toIterable[A](fa: (X, A)): Iterable[A] = toList(fa)

      override def get[A](fa: (X, A))(idx: Long): Option[A] =
        if (idx == 0L) Some(fa._2) else None

      override def exists[A](fa: (X, A))(p: A => Boolean): Boolean = p(fa._2)

      override def forall[A](fa: (X, A))(p: A => Boolean): Boolean = p(fa._2)

      override def isEmpty[A](fa: (X, A)): Boolean = false
    }
}

sealed private[instances] trait Tuple2Instances1 extends Tuple2Instances2 {
  @deprecated("Use catsStdCommutativeMonadForTuple2 in cats.instances.NTupleMonadInstances", "2.4.0")
  def catsStdCommutativeMonadForTuple2[X](implicit MX: CommutativeMonoid[X]): CommutativeMonad[(X, *)] =
    new FlatMapTuple2[X](MX) with CommutativeMonad[(X, *)] {
      def pure[A](a: A): (X, A) = (MX.empty, a)
    }
}

sealed private[instances] trait Tuple2Instances2 extends Tuple2Instances3 {
  @deprecated("Use catsStdCommutativeFlatMapForTuple2 in cats.instances.NTupleMonadInstances", "2.4.0")
  def catsStdCommutativeFlatMapForTuple2[X](implicit MX: CommutativeSemigroup[X]): CommutativeFlatMap[(X, *)] =
    new FlatMapTuple2[X](MX) with CommutativeFlatMap[(X, *)]
}

sealed private[instances] trait Tuple2Instances3 extends Tuple2Instances4 {
  @deprecated("Use catsStdMonadForTuple2 in cats.instances.NTupleMonadInstances", "2.4.0")
  def catsStdMonadForTuple2[X](implicit MX: Monoid[X]): Monad[(X, *)] =
    new FlatMapTuple2[X](MX) with Monad[(X, *)] {
      def pure[A](a: A): (X, A) = (MX.empty, a)
    }
}

sealed private[instances] trait Tuple2Instances4 {
  @deprecated("Use catsStdFlatMapForTuple2 on cats.instances.NTupleMonadInstances", "2.4.0")
  def catsStdFlatMapForTuple2[X](implicit SX: Semigroup[X]): FlatMap[(X, *)] =
    new FlatMapTuple2[X](SX)
}

private[instances] class FlatMapTuple2[X](s: Semigroup[X]) extends FlatMap[(X, *)] {
  override def ap[A, B](ff: (X, A => B))(fa: (X, A)): (X, B) = {
    val x = s.combine(ff._1, fa._1)
    val b = ff._2(fa._2)
    (x, b)
  }

  override def product[A, B](fa: (X, A), fb: (X, B)): (X, (A, B)) = {
    val x = s.combine(fa._1, fb._1)
    (x, (fa._2, fb._2))
  }

  override def map[A, B](fa: (X, A))(f: A => B): (X, B) =
    (fa._1, f(fa._2))

  def flatMap[A, B](fa: (X, A))(f: A => (X, B)): (X, B) = {
    val xb = f(fa._2)
    val x = s.combine(fa._1, xb._1)
    (x, xb._2)
  }

  override def productR[A, B](a: (X, A))(b: (X, B)): (X, B) =
    (s.combine(a._1, b._1), b._2)

  override def productL[A, B](a: (X, A))(b: (X, B)): (X, A) =
    (s.combine(a._1, b._1), a._2)

  override def mproduct[A, B](fa: (X, A))(f: A => (X, B)): (X, (A, B)) = {
    val xb = f(fa._2)
    val x = s.combine(fa._1, xb._1)
    (x, (fa._2, xb._2))
  }

  def tailRecM[A, B](a: A)(f: A => (X, Either[A, B])): (X, B) = {
    @tailrec
    def loop(x: X, aa: A): (X, B) =
      f(aa) match {
        case (nextX, Left(nextA)) => loop(s.combine(x, nextX), nextA)
        case (nextX, Right(b))    => (s.combine(x, nextX), b)
      }
    f(a) match {
      case (x, Right(b))    => (x, b)
      case (x, Left(nextA)) => loop(x, nextA)
    }
  }
}
