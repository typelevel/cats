package cats
package syntax

trait CoflatMapSyntax1 {
  implicit def coflatMapSyntaxU[FA](fa: FA)(implicit U: Unapply[CoflatMap, FA]): CoflatMapOps[U.M, U.A] =
    new CoflatMapOps[U.M, U.A](U.subst(fa))(U.TC)
}

trait CoflatMapSyntax extends CoflatMapSyntax1 {
  implicit def coflatMapSyntax[F[_]: CoflatMap, A](fa: F[A]): CoflatMapOps[F, A] =
    new CoflatMapOps(fa)
}

class CoflatMapOps[F[_], A](fa: F[A])(implicit F: CoflatMap[F]) {
  def coflatMap[B](f: F[A] => B): F[B] = F.coflatMap(fa)(f)
  def coflatten: F[F[A]] = F.coflatten(fa)
}
