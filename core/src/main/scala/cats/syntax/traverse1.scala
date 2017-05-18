package cats
package syntax

trait Traverse1Syntax extends Traverse1.ToTraverse1Ops {
  implicit def catsSyntaxNestedTraverse1[F[_]: Traverse1, G[_], A](fga: F[G[A]]): NestedTraverse1Ops[F, G, A] =
    new NestedTraverse1Ops[F, G, A](fga)
}

final class NestedTraverse1Ops[F[_], G[_], A](fga: F[G[A]])(implicit F: Traverse1[F]) {
  def reduceK(implicit G: SemigroupK[G]): G[A] = F.reduceK(fga)
}
