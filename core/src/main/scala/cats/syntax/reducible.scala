package cats
package syntax

trait ReducibleSyntax extends Reducible.ToReducibleOps {
  implicit final def catsSyntaxNestedReducible[F[_]: Reducible, G[_], A](fga: F[G[A]]): NestedReducibleOps[F, G, A] =
    new NestedReducibleOps[F, G, A](fga)
}

final class NestedReducibleOps[F[_], G[_], A](val fga: F[G[A]]) extends AnyVal {
  def reduceK(implicit F: Reducible[F], G: SemigroupK[G]): G[A] = F.reduceK(fga)
}

trait ReducibleSyntaxBinCompat0 {
  implicit final def catsSyntaxReducibleBinCompat0[F[_]: Reducible, A](fa: F[A]): ReducibleOpsBinCompat0[F, A] =
    new ReducibleOpsBinCompat0[F, A](fa)
}

final class ReducibleOpsBinCompat0[F[_], A](val fa: F[A]) extends AnyVal {
  def maxBy[B](f: A => B)(implicit F: Reducible[F], B: Order[B]): A =
    F.reduceLeft[A](fa)((a1, a2) => if (B.gteqv(f(a1), f(a2))) a1 else a2)

  def minBy[B](f: A => B)(implicit F: Reducible[F], B: Order[B]): A =
    F.reduceLeft[A](fa)((a1, a2) => if (B.lteqv(f(a1), f(a2))) a1 else a2)
}

