package cats
package syntax

trait FlatMapSyntax {
  // TODO: use simulacrum instances eventually
  implicit def flatMapSyntax[F[_]: FlatMap, A](fa: F[A]) =
    new FlatMapOps[F, A](fa)

  implicit def flattenSyntax[F[_]: FlatMap, A](ffa: F[F[A]]) =
    new FlattenOps[F, A](ffa)
}

class FlatMapOps[F[_], A](fa: F[A])(implicit F: FlatMap[F]) {
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)
}

class FlattenOps[F[_], A](ffa: F[F[A]])(implicit F: FlatMap[F]) {
  def flatten: F[A] = F.flatten(ffa)
}
