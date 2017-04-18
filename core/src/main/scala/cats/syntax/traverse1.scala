package cats
package syntax

private[syntax] trait Traverse1Syntax1 {
  implicit def catsSyntaxUTraverse1[FA](fa: FA)(implicit U: Unapply[Traverse1, FA]): Traverse1.Ops[U.M, U.A] =
    new Traverse1.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait Traverse1Syntax extends Traverse1.ToTraverse1Ops with Traverse1Syntax1 {
  implicit def catsSyntaxNestedTraverse1[F[_]: Traverse1, G[_], A](fga: F[G[A]]): NestedTraverse1Ops[F, G, A] =
    new NestedTraverse1Ops[F, G, A](fga)
}

final class NestedTraverse1Ops[F[_], G[_], A](fga: F[G[A]])(implicit F: Traverse1[F]) {
  def reduceK(implicit G: SemigroupK[G]): G[A] = F.reduceK(fga)
}
