package cats
package syntax

private[syntax] trait FoldableSyntax1 {
  implicit def catsSyntaxUFoldable[FA](fa: FA)(implicit U: Unapply[Foldable, FA]): Foldable.Ops[U.M, U.A] =
    new Foldable.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
      }
}

trait FoldableSyntax extends Foldable.ToFoldableOps with FoldableSyntax1 {
  implicit def catsSyntaxNestedFoldable[F[_]: Foldable, G[_], A](fga: F[G[A]]): NestedFoldableOps[F, G, A] =
    new NestedFoldableOps[F, G, A](fga)
}

final class NestedFoldableOps[F[_], G[_], A](fga: F[G[A]])(implicit F: Foldable[F]) {
  def sequence_(implicit G: Applicative[G]): G[Unit] = F.sequence_(fga)

  /**
   * @see [[Foldable.foldK]].
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val l: List[Set[Int]] = List(Set(1, 2), Set(2, 3), Set(3, 4))
   * scala> l.foldK
   * res0: Set[Int] = Set(1, 2, 3, 4)
   * }}}
   */
  def foldK(implicit G: MonoidK[G]): G[A] = F.foldK(fga)
}
