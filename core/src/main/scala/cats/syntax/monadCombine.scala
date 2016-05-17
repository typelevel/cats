package cats
package syntax

trait MonadCombineSyntax {
  // TODO: use simulacrum instances eventually
  implicit def monadCombineSyntax[F[_]: MonadCombine, G[_], A](fga: F[G[A]]): MonadCombineOps[F, G, A] =
    new MonadCombineOps[F, G, A](fga)

  implicit def separateSyntax[F[_]: MonadCombine, G[_, _], A, B](fgab: F[G[A, B]]): SeparateOps[F, G, A, B] =
    new SeparateOps[F, G, A, B](fgab)
}

final class MonadCombineOps[F[_], G[_], A](fga: F[G[A]])(implicit F: MonadCombine[F]) {

  /**
   * @see [[MonadCombine.unite]]
   *
   * Example:
   * {{{
   * scala> import cats.std.list._
   * scala> import cats.std.vector._
   * scala> import cats.syntax.monadCombine._
   * scala> val x: List[Vector[Int]] = List(Vector(1, 2), Vector(3, 4))
   * scala> x.unite
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def unite(implicit G: Foldable[G]): F[A] = F.unite(fga)
}

final class SeparateOps[F[_], G[_, _], A, B](fgab: F[G[A, B]])(implicit F: MonadCombine[F]) {

  /**
   * @see [[MonadCombine.separate]]
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.std.list._
   * scala> import cats.syntax.monadCombine._
   * scala> val l: List[Xor[String, Int]] = List(Xor.right(1), Xor.left("error"))
   * scala> l.separate
   * res0: (List[String], List[Int]) = (List(error),List(1))
   * }}}
   */
  def separate(implicit G: Bifoldable[G]): (F[A], F[B]) = F.separate(fgab)
}