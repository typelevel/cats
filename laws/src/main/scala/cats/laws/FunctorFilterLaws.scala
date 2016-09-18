package cats
package laws

import cats.implicits._

trait FunctorFilterLaws[F[_]] extends FunctorLaws[F] {
  implicit def F0: FunctorFilter[F]
  implicit def F: Functor[F] = F0.functorInstance

  def mapFilterComposition[A, B, C](
    fa: F[A],
    f: A => Option[B],
    g: B => Option[C]
  ): IsEq[F[C]] = {

    val lhs: F[C] = fa.mapFilter(f).mapFilter(g)
    val rhs: F[C] = fa.mapFilter(a => f(a).flatMap(g))
    lhs <-> rhs
  }

  /**
   * Combined with the functor identity law, this implies a `mapFilter` identity
   * law (when `f` is the identity function).
   */
  def mapFilterMapConsistency[A, B](
    fa: F[A],
    f: A => B
  ): IsEq[F[B]] = {
    fa.mapFilter(f andThen (_.some)) <-> fa.map(f)
  }
}

object FunctorFilterLaws {
  def apply[F[_]](implicit ev: FunctorFilter[F]): FunctorFilterLaws[F] =
    new FunctorFilterLaws[F] { def F0: FunctorFilter[F] = ev }
}
