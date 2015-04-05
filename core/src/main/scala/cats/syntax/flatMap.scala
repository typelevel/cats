package cats
package syntax

trait FlatMapSyntax {
  // TODO: use simulacrum instances eventually
  implicit def flatMapSyntax[F[_]: FlatMap, A](fa: F[A]): FlatMapOps[F, A] =
    new FlatMapOps[F, A](fa)

  implicit def flattenSyntax[F[_]: FlatMap, A](ffa: F[F[A]]): FlattenOps[F, A] =
    new FlattenOps[F, A](ffa)

  implicit def ifMSyntax[F[_]: FlatMap](fa: F[Boolean]): IfMOps[F] =
    new IfMOps[F](fa)
}

class FlatMapOps[F[_], A](fa: F[A])(implicit F: FlatMap[F]) {
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)
  def mproduct[B](f: A => F[B]): F[(A, B)] = F.mproduct(fa)(f)
}

class FlattenOps[F[_], A](ffa: F[F[A]])(implicit F: FlatMap[F]) {
  def flatten: F[A] = F.flatten(ffa)
}

class IfMOps[F[_]](fa: F[Boolean])(implicit F: FlatMap[F]) {
  def ifM[B](ifTrue: => F[B], ifFalse: => F[B]): F[B] = F.ifM(fa)(ifTrue, ifFalse)
}
