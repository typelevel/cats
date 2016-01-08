package cats
package syntax

trait MonadCombineSyntax {
  // TODO: use simulacrum instances eventually
  implicit def nestedMonadCombineSyntax[F[_]: MonadCombine, G[_], A](fga: F[G[A]]): NestedMonadCombineOps[F, G, A] =
    new NestedMonadCombineOps[F, G, A](fga)
}

final class NestedMonadCombineOps[F[_], G[_], A](fga: F[G[A]])(implicit F: MonadCombine[F]) {

  /**
   * @see [[MonadCombine.unite]]
   *
   * Example:
   * {{{
   * scala> import cats.data.Streaming
   * scala> import cats.std.list._
   * scala> import cats.syntax.monadCombine._
   * scala> val x: List[Streaming[Int]] = List(Streaming(1, 2), Streaming(3, 4))
   * scala> x.unite
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def unite(implicit G: Foldable[G]): F[A] = F.unite(fga)
}
