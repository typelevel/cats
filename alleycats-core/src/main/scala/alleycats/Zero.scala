package alleycats

import cats.Eq
import cats.syntax.eq._

import simulacrum.typeclass

@typeclass trait Zero[A] {
  def zero: A

  def isZero(a: A)(implicit ev: Eq[A]): Boolean =
    zero === a

  def nonZero(a: A)(implicit ev: Eq[A]): Boolean =
    zero =!= a
}

object Zero {
  def apply[A](a: => A): Zero[A] =
    new Zero[A] { lazy val zero: A = a }
}
