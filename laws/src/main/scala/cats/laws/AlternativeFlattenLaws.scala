package cats
package laws

import cats.implicits._

trait AlternativeFlattenLaws[F[_]] extends FunctorFilterLaws[F] with AlternativeLaws[F] {
  implicit override def F: AlternativeFlatten[F]

  def mapFlattenComposition[G1[_]: Traverse, G2[_]: Traverse, A, B, C](
    fa: F[A],
    f: A => G1[B],
    g: B => G2[C]
  ): IsEq[F[C]] = {

    val lhs: F[C] = fa.mapFlatten(f).mapFlatten(g)
    implicit val composed: Traverse[λ[α => G1[G2[α]]]] = Traverse[G1].compose[G2]

    val rhs: F[C] = fa.mapFlatten[λ[α => G1[G2[α]]], C](a => f(a).map(g))
    lhs <-> rhs
  }

  def traverseConsistency0[G[_]: Traverse, A](
    ga: G[A]
  ): IsEq[F[A]] = {
    val G = implicitly[Traverse[G]]
    val lhs = F.flattenT(G.traverse(ga)(F.pure))
    val rhs = G.foldLeft(ga, F.empty[A]) { (fa, a) => F.combineK(fa, F.pure(a)) }
    lhs <-> rhs
  }
  def traverseConsistency1[G[_]: Traverse, A](
    ga: G[A]
  ): IsEq[F[A]] = {
    val G = implicitly[Traverse[G]]
    val lhs = F.fromTraverse(ga)
    val rhs = G.foldLeft(ga, F.empty[A]) { (fa, a) => F.combineK(fa, F.pure(a)) }
    lhs <-> rhs
  }
}

object AlternativeFlattenLaws {
  def apply[F[_]](implicit ev: AlternativeFlatten[F]): AlternativeFlattenLaws[F] =
    new AlternativeFlattenLaws[F] { def F: AlternativeFlatten[F] = ev }
}
