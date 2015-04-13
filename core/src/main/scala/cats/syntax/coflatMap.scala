package cats
package syntax

trait CoflatMapSyntax {
  // TODO: use simulacrum instances eventually
  implicit def coflatMapSyntax[FA](fa: FA)(implicit U: Unapply[CoflatMap, FA]): CoflatMapOps[U.M, U.A] =
    new CoflatMapOps[U.M, U.A](U.subst(fa))(U.TC)
}

class CoflatMapOps[F[_], A](fa: F[A])(implicit F: CoflatMap[F]) {
  def coflatMap[B](f: F[A] => B): F[B] = F.coflatMap(fa)(f)
  def coflatten: F[F[A]] = F.coflatten(fa)
}
