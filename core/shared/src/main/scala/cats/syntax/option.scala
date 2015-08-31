package cats
package syntax

import cats.data.Xor

trait OptionSyntax {
  def none[A] = Option.empty[A]
  implicit def optionIdSyntax[A](a: A): OptionIdOps[A] = new OptionIdOps(a)
  implicit def optionSyntax[A](oa: Option[A]): OptionOps[A] = new OptionOps(oa)
}

class OptionIdOps[A](val a: A) extends AnyVal {
  def some: Option[A] = Option(a)
}

class OptionOps[A](val oa: Option[A]) extends AnyVal {
  def toLeftXor[B](b: => B): A Xor B = oa.fold[A Xor B](Xor.Right(b))(Xor.Left(_))
  def toRightXor[B](b: => B): B Xor A = oa.fold[B Xor A](Xor.Left(b))(Xor.Right(_))
  def orEmpty(implicit A: Monoid[A]): A = oa.getOrElse(A.empty)
}
