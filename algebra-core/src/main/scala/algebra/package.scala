package object algebra {

  type Band[A] = cats.kernel.Band[A]
  val Band = cats.kernel.Band

  type BoundedSemilattice[A] = cats.kernel.BoundedSemilattice[A]
  val BoundedSemilattice = cats.kernel.BoundedSemilattice

  type CommutativeGroup[A] = cats.kernel.CommutativeGroup[A]
  val CommutativeGroup = cats.kernel.CommutativeGroup

  type CommutativeMonoid[A] = cats.kernel.CommutativeMonoid[A]
  val CommutativeMonoid = cats.kernel.CommutativeMonoid

  type CommutativeSemigroup[A] = cats.kernel.CommutativeSemigroup[A]
  val CommutativeSemigroup = cats.kernel.CommutativeSemigroup

  type Eq[A] = cats.kernel.Eq[A]
  val Eq = cats.kernel.Eq

  type Group[A] = cats.kernel.Group[A]
  val Group = cats.kernel.Group

  type Monoid[A] = cats.kernel.Monoid[A]
  val Monoid = cats.kernel.Monoid

  type Order[A] = cats.kernel.Order[A]
  val Order = cats.kernel.Order

  type PartialOrder[A] = cats.kernel.PartialOrder[A]
  val PartialOrder = cats.kernel.PartialOrder

  type Semigroup[A] = cats.kernel.Semigroup[A]
  val Semigroup = cats.kernel.Semigroup

  type Semilattice[A] = cats.kernel.Semilattice[A]
  val Semilattice = cats.kernel.Semilattice
}
