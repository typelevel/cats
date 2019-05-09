package alleycats

import cats.{Eq, Monoid}
import cats.syntax.eq._

import simulacrum.typeclass

@typeclass trait Empty[A] {
  def empty: A

  def isEmpty(a: A)(implicit ev: Eq[A]): Boolean =
    empty === a

  def nonEmpty(a: A)(implicit ev: Eq[A]): Boolean =
    empty =!= a
}

object Empty extends EmptyInstances0 {
  def apply[A](a: => A): Empty[A] =
    new Empty[A] { lazy val empty: A = a }

  def fromEmptyK[F[_], T](implicit ekf: EmptyK[F]): Empty[F[T]] = ekf.synthesize[T]
}

trait EmptyInstances0 extends compat.IterableEmptyInstance with EmptyInstances1

trait EmptyInstances1 {
  // If Monoid extended Empty then this could be an exported subclass instance provided by Monoid
  implicit def monoidIsEmpty[A: Monoid]: Empty[A] =
    Empty(Monoid[A].empty)
}
