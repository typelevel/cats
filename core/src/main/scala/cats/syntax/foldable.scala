package cats
package syntax

trait FoldableSyntax extends Foldable.ToFoldableOps with UnorderedFoldable.ToUnorderedFoldableOps {
  implicit final def catsSyntaxNestedFoldable[F[_]: Foldable, G[_], A](fga: F[G[A]]): NestedFoldableOps[F, G, A] =
    new NestedFoldableOps[F, G, A](fga)

  implicit final def catsSyntaxFoldOps[F[_]: Foldable, A](fa: F[A]): FoldableOps[F, A] =
    new FoldableOps[F, A](fa)
}

final class NestedFoldableOps[F[_], G[_], A](val fga: F[G[A]]) extends AnyVal {
  def sequence_(implicit F: Foldable[F], G: Applicative[G]): G[Unit] = F.sequence_(fga)

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
  def foldK(implicit F: Foldable[F], G: MonoidK[G]): G[A] = F.foldK(fga)
}

final class FoldableOps[F[_], A](val fa: F[A]) extends AnyVal {
  def foldl[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.foldLeft(fa, b)(f)

  def foldr[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
    F.foldRight(fa, b)(f)

  def contains(v: A)(implicit F: Foldable[F], ev: Eq[A]): Boolean =
    F.contains(fa)(v)

  def foldSmash(prefix: A, delim: A, suffix: A)(implicit F: Foldable[F], ev: Monoid[A]): A =
    F.foldSmash(fa)(prefix, delim, suffix)

  def mkString(prefix: String, delim: String, suffix: String)
              (implicit F: Foldable[F], ev1: Functor[F], ev2: Show[A], ev3: Monoid[A], ev4: Monoid[String]): String =
    F.mkString(fa)(prefix, delim, suffix)
}
