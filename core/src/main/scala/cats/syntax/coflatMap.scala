package cats
package syntax

trait CoflatMapSyntax {
  // TODO: use simulacrum instances eventually
  implicit def coflatMapSyntax[F[_]: CoflatMap, A](fa: F[A]): CoflatMapOps[F, A] =
    new CoflatMapOps[F, A](fa)
}

class CoflatMapOps[F[_], A](fa: F[A])(implicit F: CoflatMap[F]) {
  def coflatMap[B](f: F[A] => B): F[B] = F.coflatMap(fa)(f)
  def coflatten: F[F[A]] = F.coflatten(fa)
}
