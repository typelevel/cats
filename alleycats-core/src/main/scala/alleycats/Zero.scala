package alleycats

import cats.Eq
import cats.syntax.eq._
import export.imports
import simulacrum.typeclass

@typeclass trait Zero[A] {
  def zero: A

  def isZero(a: A)(implicit ev: Eq[A]): Boolean =
    zero === a

  def nonZero(a: A)(implicit ev: Eq[A]): Boolean =
    zero =!= a
}

object Zero extends Zero0 {
  def apply[A](a: => A): Zero[A] =
    new Zero[A] { lazy val zero: A = a }
}

@imports[Zero]
trait Zero0
