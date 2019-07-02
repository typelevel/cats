package alleycats

import cats.Eq
import cats.syntax.eq._
import simulacrum.typeclass

@typeclass trait One[A] {
  def one: A

  def isOne(a: A)(implicit ev: Eq[A]): Boolean =
    one === a

  def nonOne(a: A)(implicit ev: Eq[A]): Boolean =
    one =!= a
}

object One {
  def apply[A](a: => A): One[A] =
    new One[A] { lazy val one: A = a }
}
