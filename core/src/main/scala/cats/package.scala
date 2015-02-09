/**
 * Symbolic aliases for various types are defined here.
 */
package object cats {
  /**
   * Convenient alias to make identity instances well-kinded.
   *
   * For instance, [[cats.Functor]] expects a type of kind
   * `* -> *`, but values have a type of kind `*`. This alias
   * allows us to define `Functor[Id]` which means the same thing.
   */
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
