package cats

import simulacrum.typeclass

@typeclass trait AlternativeFlatten[F[_]] extends Alternative[F] with FunctorFilter[F] {
  /**
   * A generalized [[map]] followed by flatten.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Int] = List(1, 2, 3)
   * scala> l.mapFlatten(i => Vector.fill(i)(i))
   * res0: List[Int] = List(1, 2, 2, 3, 3, 3)
   * }}}
   */
  def mapFlatten[G[_]: Traverse, A, B](fa: F[A])(f: A => G[B]): F[B]

  def flattenT[G[_]: Traverse, A](fa: F[G[A]]): F[A] =
    mapFlatten(fa)(identity)

  def fromTraverse[G[_]: Traverse, A](fa: G[A]): F[A] =
    flattenT(pure(fa))

  override def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B] = {
    import cats.instances.option._
    mapFlatten[Option, A, B](fa)(f)
  }
}
