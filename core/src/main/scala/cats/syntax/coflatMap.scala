package cats
package syntax

trait CoFlatMapSyntax {
  // TODO: use simulacrum instances eventually
  implicit def coflatMapSyntax[F[_]: CoFlatMap, A](fa: F[A]) =
    new CoFlatMapOps[F, A](fa)
}

class CoFlatMapOps[F[_], A](fa: F[A])(implicit F: CoFlatMap[F]) {
  def coflatMap[B](f: F[A] => B): F[B] = F.coflatMap(fa)(f)
  def coflatten: F[F[A]] = F.coflatten(fa)
}
