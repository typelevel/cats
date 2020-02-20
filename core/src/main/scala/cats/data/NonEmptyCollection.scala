package cats.data

import cats.Show
import cats.kernel.{Order, Semigroup}

private[cats] trait NonEmptyCollection[+A, U[+_], NE[+_]] extends Any {
  def head: A
  def tail: U[A]
  def last: A
  def init: U[A]

  def iterator: Iterator[A]

  def map[B](f: A => B): NE[B]
  def reverse: NE[A]
  def prepend[AA >: A](a: AA): NE[AA]
  def append[AA >: A](a: AA): NE[AA]

  def filter(f: A => Boolean): U[A]
  def filterNot(p: A => Boolean): U[A]
  def collect[B](pf: PartialFunction[A, B]): U[B]
  def find(p: A => Boolean): Option[A]
  def exists(p: A => Boolean): Boolean
  def forall(p: A => Boolean): Boolean

  def foldLeft[B](b: B)(f: (B, A) => B): B
  def reduce[AA >: A](implicit S: Semigroup[AA]): AA

  def zipWith[B, C](b: NE[B])(f: (A, B) => C): NE[C]
  def zipWithIndex: NE[(A, Int)]

  def distinct[AA >: A](implicit O: Order[AA]): NE[AA]
  def sortBy[B](f: A => B)(implicit B: Order[B]): NE[A]
  def sorted[AA >: A](implicit AA: Order[AA]): NE[AA]
  def groupByNem[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NE[A]]
  def toNem[T, V](implicit ev: A <:< (T, V), order: Order[T]): NonEmptyMap[T, V]
  def toNes[B >: A](implicit order: Order[B]): NonEmptySet[B]

  def show[AA >: A](implicit AA: Show[AA]): String
}
