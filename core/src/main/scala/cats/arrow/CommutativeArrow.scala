package cats
package arrow

import simulacrum.typeclass

/**
 * In a Commutative Arrow F[_, _], the split operation (or `***`) is commutative,
 * which means that there is non-interference between the effect of the paired arrows.
 *
 * Must obey the laws in CommutativeArrowLaws
 */
@typeclass trait CommutativeArrow[F[_, _]] extends Arrow[F]
