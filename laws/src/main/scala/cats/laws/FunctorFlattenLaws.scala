package cats
package laws

import cats.implicits._

trait FunctorFlattenLaws[F[_]] extends FunctorFilterLaws[F] {
  implicit override def F: FunctorFlatten[F]

  def mapFlattenComposition[G1[_]: Foldable: Functor, G2[_]: Foldable, A, B, C](
    fa: F[A],
    f: A => G1[B],
    g: B => G2[C]
  ): IsEq[F[C]] = {

    val lhs: F[C] = fa.mapFlatten(f).mapFlatten(g)
    implicit val composed: Foldable[λ[α => G1[G2[α]]]] = Foldable[G1].compose[G2]

    val rhs: F[C] = fa.mapFlatten[λ[α => G1[G2[α]]], C](a => f(a).map(g))
    lhs <-> rhs
  }

  /**
   * Combined with the functor identity law, this implies a `mapFilter` identity
   * law (when `f` is the identity function).
   */
  def mapFlattenMapConsistency[A, B](
    fa: F[A],
    f: A => B
  ): IsEq[F[B]] = {
    fa.mapFlatten(f andThen (List(_))) <-> fa.map(f)
  }
}

object FunctorFlattenLaws {
  def apply[F[_]](implicit ev: FunctorFlatten[F]): FunctorFlattenLaws[F] =
    new FunctorFlattenLaws[F] { def F: FunctorFlatten[F] = ev }
}
