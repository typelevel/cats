package cats
package syntax

import cats.data.Xor

trait OptionSyntax {
  def none[A] = Option.empty[A]
  implicit def toOptionSyntax[A](a: A): ToOptionOps[A] = new ToOptionOps(a)
  implicit def optionSyntax[A](a: Option[A]): OptionOps[A] = new OptionOps(a)
}

class ToOptionOps[A](val a: A) extends AnyVal {
  def some: Option[A] = Option(a)
}

class OptionOps[A](val a: Option[A]) extends AnyVal {
  def toLeftXor[B](b: => B): A Xor B = a.fold[A Xor B](Xor.Right(b))(Xor.Left(_))
  def toRightXor[B](b: => B): B Xor A = a.fold[B Xor A](Xor.Left(b))(Xor.Right(_))
  def orEmpty(implicit A: Monoid[A]): A = a.getOrElse(A.empty)
}
