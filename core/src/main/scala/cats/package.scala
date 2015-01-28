/**
 * Symbolic aliases for various types are defined here.
 */
package object cats {
  type Id[A] = A

  type <=>[A, B] = Bijection[A, B]

  type ~>[F[_], G[_]] = arrow.NaturalTransformation[F, G]
  type <~[F[_], G[_]] = arrow.NaturalTransformation[G, F]

  type ⊥ = Nothing
  type ⊤ = Any
}
