package cats
package syntax

trait MonadCombineSyntax {
  // TODO: use simulacrum instances eventually
  implicit final def catsSyntaxMonadCombine[F[_]: MonadCombine, G[_], A](fga: F[G[A]]): MonadCombineOps[F, G, A] =
    new MonadCombineOps[F, G, A](fga)

  implicit final def catsSyntaxMonadCombineSeparate[F[_]: MonadCombine, G[_, _], A, B](fgab: F[G[A, B]]): SeparateOps[F, G, A, B] =
    new SeparateOps[F, G, A, B](fgab)
}

final class MonadCombineOps[F[_], G[_], A](val fga: F[G[A]]) extends AnyVal {

  /**
   * @see [[MonadCombine.unite]]
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val x: List[Vector[Int]] = List(Vector(1, 2), Vector(3, 4))
   * scala> x.unite
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def unite(implicit F: MonadCombine[F], G: Foldable[G]): F[A] = F.unite(fga)
}

final class SeparateOps[F[_], G[_, _], A, B](val fgab: F[G[A, B]]) extends AnyVal {

  /**
   * @see [[MonadCombine.separate]]
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Either[String, Int]] = List(Right(1), Left("error"))
   * scala> l.separate
   * res0: (List[String], List[Int]) = (List(error),List(1))
   * }}}
   */
  def separate(implicit F: MonadCombine[F], G: Bifoldable[G]): (F[A], F[B]) = F.separate(fgab)
}
