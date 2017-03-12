package cats
package syntax

private[syntax] trait ReducibleSyntax1 {
  implicit def catsSyntaxUReducible[FA](fa: FA)(implicit U: Unapply[Reducible, FA]): Reducible.Ops[U.M, U.A] =
    new Reducible.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait ReducibleSyntax extends Reducible.ToReducibleOps with ReducibleSyntax1 {
  implicit def catsSyntaxNestedReducible[F[_]: Reducible, G[_], A](fga: F[G[A]]): NestedReducibleOps[F, G, A] =
    new NestedReducibleOps[F, G, A](fga)
}

final class NestedReducibleOps[F[_], G[_], A](fga: F[G[A]])(implicit F: Reducible[F]) {
  def reduceK(implicit G: SemigroupK[G]): G[A] = F.reduceK(fga)
}
