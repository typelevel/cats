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
package laws

import cats.syntax.all._

import scala.collection.mutable

trait FoldableLaws[F[_]] extends UnorderedFoldableLaws[F] {
  implicit def F: Foldable[F]

  def foldRightLazy[A](fa: F[A]): Boolean = {
    var i = 0
    F.foldRight(fa, Eval.now("empty")) { (_, _) =>
      i += 1
      Eval.now("not empty")
    }.value
    i == (if (F.isEmpty(fa)) 0 else 1)
  }

  def leftFoldConsistentWithFoldMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit M: Monoid[B]): IsEq[B] =
    fa.foldMap(f) <-> fa.foldLeft(M.empty) { (b, a) =>
      b |+| f(a)
    }

  def rightFoldConsistentWithFoldMap[A, B](
    fa: F[A],
    f: A => B
  )(implicit M: Monoid[B]): IsEq[B] =
    fa.foldMap(f) <-> fa.foldRight(Later(M.empty))((a, lb) => lb.map(f(a) |+| _)).value

  def existsConsistentWithFind[A](fa: F[A], p: A => Boolean): Boolean =
    F.exists(fa)(p) == F.find(fa)(p).isDefined

  /**
   * Monadic folding with identity monad is analogous to `foldLeft`.
   */
  def foldMIdentity[A, B](
    fa: F[A],
    b: B,
    f: (B, A) => B
  ): IsEq[B] =
    F.foldM[Id, A, B](fa, b)(f) <-> F.foldLeft(fa, b)(f)

  def foldRightDeferConsistentWithFoldRight[A, B](
    fa: F[A],
    f: (B, A) => B
  )(implicit M: Monoid[B]): IsEq[B] = {
    val g: (A, Eval[B]) => Eval[B] = (a, ea) => ea.map(f(_, a))

    F.foldRight(fa, Later(M.empty))(g).value <-> F.foldRightDefer(fa, Later(M.empty): Eval[B])(g).value
  }

  /**
   * `reduceLeftOption` consistent with `reduceLeftToOption`
   */
  def reduceLeftOptionConsistentWithReduceLeftToOption[A](
    fa: F[A],
    f: (A, A) => A
  ): IsEq[Option[A]] =
    F.reduceLeftOption(fa)(f) <-> F.reduceLeftToOption(fa)(identity)(f)

  /**
   * `reduceRightOption` consistent with `reduceRightToOption`
   */
  def reduceRightOptionConsistentWithReduceRightToOption[A](
    fa: F[A],
    f: (A, A) => A
  ): IsEq[Option[A]] = {
    val g: (A, Eval[A]) => Eval[A] = (a, ea) => ea.map(f(a, _))
    F.reduceRightOption(fa)(g).value <-> F.reduceRightToOption(fa)(identity)(g).value
  }

  def getRef[A](fa: F[A], idx: Long): IsEq[Option[A]] =
    F.get(fa)(idx) <-> (if (idx < 0L) None
                        else
                          F.foldM[Either[A, *], A, Long](fa, 0L) { (i, a) =>
                            if (i == idx) Left(a) else Right(i + 1L)
                          } match {
                            case Left(a)  => Some(a)
                            case Right(_) => None
                          })

  def foldRef[A](fa: F[A])(implicit A: Monoid[A]): IsEq[A] =
    F.fold(fa) <-> F.foldLeft(fa, A.empty) { (acc, a) =>
      A.combine(acc, a)
    }

  def toListRef[A](fa: F[A]): IsEq[List[A]] =
    F.toList(fa) <-> F
      .foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
        buf += a
      }
      .toList

  def filter_Ref[A](fa: F[A], p: A => Boolean): IsEq[List[A]] =
    F.filter_(fa)(p) <-> F
      .foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
        if (p(a)) buf += a else buf
      }
      .toList

  def takeWhile_Ref[A](fa: F[A], p: A => Boolean): IsEq[List[A]] =
    F.takeWhile_(fa)(p) <-> F
      .foldRight(fa, Now(List.empty[A])) { (a, llst) =>
        if (p(a)) llst.map(a :: _) else Now(Nil)
      }
      .value

  def dropWhile_Ref[A](fa: F[A], p: A => Boolean): IsEq[List[A]] =
    F.dropWhile_(fa)(p) <-> F
      .foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
        if (buf.nonEmpty || !p(a)) buf += a else buf
      }
      .toList

  def collectFirstSome_Ref[A, B](fa: F[A], f: A => Option[B]): IsEq[Option[B]] =
    F.collectFirstSome(fa)(f) <-> F.foldLeft(fa, Option.empty[B]) { (ob, a) =>
      if (ob.isDefined) ob else f(a)
    }

  def collectFirst_Ref[A, B](fa: F[A], pf: PartialFunction[A, B]): IsEq[Option[B]] =
    F.collectFirst(fa)(pf) <-> F.collectFirstSome(fa)(pf.lift)

  def orderedConsistency[A: Eq](x: F[A], y: F[A])(implicit ev: Eq[F[A]]): IsEq[List[A]] =
    if (x === y) F.toList(x) <-> F.toList(y)
    else List.empty[A] <-> List.empty[A]

}

object FoldableLaws {
  def apply[F[_]](implicit ev: Foldable[F]): FoldableLaws[F] =
    new FoldableLaws[F] { def F: Foldable[F] = ev }
}
