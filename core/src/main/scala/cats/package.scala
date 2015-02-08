/**
 * Symbolic aliases for various types are defined here.
 */
package object cats {
  type Id[A] = A

  type ~>[F[_], G[_]] = arrow.NaturalTransformation[F, G]
  type <~[F[_], G[_]] = arrow.NaturalTransformation[G, F]

  type ⊥ = Nothing
  type ⊤ = Any

  type Eq[A] = algebra.Eq[A]
  type PartialOrder[A] = algebra.PartialOrder[A]
  type Order[A] = algebra.Order[A]
  type Semigroup[A] = algebra.Semigroup[A]
  type Monoid[A] = algebra.Monoid[A]

  val Eq = algebra.Eq
  val PartialOrder = algebra.PartialOrder
  val Order = algebra.Order
  val Semigroup = algebra.Semigroup
  val Monoid = algebra.Monoid
}
