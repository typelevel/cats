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

abstract private[data] class AbstractNonEmptyInstances[F[_], NonEmptyF[_]](implicit
  MF: Monad[F],
  CF: CoflatMap[F],
  TF: Traverse[F],
  SF: Alternative[F]
) extends Bimonad[NonEmptyF]
    with NonEmptyTraverse[NonEmptyF]
    with NonEmptyAlternative[NonEmptyF] {
  val monadInstance = MF.asInstanceOf[Monad[NonEmptyF]]
  val coflatMapInstance = CF.asInstanceOf[CoflatMap[NonEmptyF]]
  val traverseInstance = Traverse[F].asInstanceOf[Traverse[NonEmptyF]]
  val alternativeInstance = Alternative[F].asInstanceOf[Alternative[NonEmptyF]]

  def combineK[A](a: NonEmptyF[A], b: NonEmptyF[A]): NonEmptyF[A] =
    alternativeInstance.combineK(a, b)

  override def prependK[A](a: A, fa: NonEmptyF[A]): NonEmptyF[A] =
    alternativeInstance.prependK(a, fa)

  override def appendK[A](fa: NonEmptyF[A], a: A): NonEmptyF[A] =
    alternativeInstance.appendK(fa, a)

  def pure[A](x: A): NonEmptyF[A] = monadInstance.pure(x)

  override def map[A, B](fa: NonEmptyF[A])(f: A => B): NonEmptyF[B] = monadInstance.map(fa)(f)

  def flatMap[A, B](fa: NonEmptyF[A])(f: A => NonEmptyF[B]): NonEmptyF[B] =
    monadInstance.flatMap(fa)(f)

  override def map2[A, B, Z](fa: NonEmptyF[A], fb: NonEmptyF[B])(f: (A, B) => Z): NonEmptyF[Z] =
    monadInstance.map2(fa, fb)(f)

  override def map2Eval[A, B, Z](fa: NonEmptyF[A], fb: Eval[NonEmptyF[B]])(f: (A, B) => Z): Eval[NonEmptyF[Z]] =
    monadInstance.map2Eval(fa, fb)(f)

  def coflatMap[A, B](fa: NonEmptyF[A])(f: NonEmptyF[A] => B): NonEmptyF[B] =
    coflatMapInstance.coflatMap(fa)(f)

  def tailRecM[A, B](a: A)(f: A => NonEmptyF[Either[A, B]]): NonEmptyF[B] =
    monadInstance.tailRecM(a)(f)

  def foldLeft[A, B](fa: NonEmptyF[A], b: B)(f: (B, A) => B): B =
    traverseInstance.foldLeft(fa, b)(f)

  def foldRight[A, B](fa: NonEmptyF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    traverseInstance.foldRight(fa, lb)(f)

  override def foldMap[A, B](fa: NonEmptyF[A])(f: A => B)(implicit B: Monoid[B]): B =
    traverseInstance.foldMap(fa)(f)

  override def traverse[G[_], A, B](fa: NonEmptyF[A])(f: A => G[B])(implicit G: Applicative[G]): G[NonEmptyF[B]] =
    traverseInstance.traverse(fa)(f)

  override def mapWithIndex[A, B](fa: NonEmptyF[A])(f: (A, Int) => B): NonEmptyF[B] =
    traverseInstance.mapWithIndex(fa)(f)

  override def zipWithIndex[A](fa: NonEmptyF[A]): NonEmptyF[(A, Int)] = traverseInstance.zipWithIndex(fa)

  override def exists[A](fa: NonEmptyF[A])(p: A => Boolean): Boolean = traverseInstance.exists(fa)(p)

  override def forall[A](fa: NonEmptyF[A])(p: A => Boolean): Boolean = traverseInstance.forall(fa)(p)

  override def get[A](fa: NonEmptyF[A])(idx: Long): Option[A] = traverseInstance.get(fa)(idx)

  override def isEmpty[A](fa: NonEmptyF[A]): Boolean = false

  override def foldM[G[_], A, B](fa: NonEmptyF[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] =
    traverseInstance.foldM(fa, z)(f)

  override def fold[A](fa: NonEmptyF[A])(implicit A: Monoid[A]): A = traverseInstance.fold(fa)

  override def toList[A](fa: NonEmptyF[A]): List[A] = traverseInstance.toList(fa)

  override def reduceLeftOption[A](fa: NonEmptyF[A])(f: (A, A) => A): Option[A] =
    traverseInstance.reduceLeftOption(fa)(f)

  override def find[A](fa: NonEmptyF[A])(f: A => Boolean): Option[A] = traverseInstance.find(fa)(f)

  override def collectFirst[A, B](fa: NonEmptyF[A])(pf: PartialFunction[A, B]): Option[B] =
    traverseInstance.collectFirst(fa)(pf)

  override def collectFirstSome[A, B](fa: NonEmptyF[A])(f: A => Option[B]): Option[B] =
    traverseInstance.collectFirstSome(fa)(f)
}
