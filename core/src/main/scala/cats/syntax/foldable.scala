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
  
   /**
   * test if `F[A]` contains an `A`, named contains_ to avoid conflict with existing contains which uses universal equality
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val l: List[Int] = List(1, 2, 3, 4)
   * scala> l.contains_(1)
   * res0: Boolean = true
   * scala> l.contains_(5)
   * res1: Boolean = false
   * }}}
   */
  def contains_[A](v: A)(implicit ev: Eq[A], F: Foldable[F]): Boolean =
    F.exists(fa)(ev.eqv(_, v))

  
   /**
   * Intercalate with a prefix and a suffix
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val l: List[Int] = List("1", "2", "3")
   * scala> l.foldSmash("List(", ",", ")")
   * res0: String = List(1,2,3)
   * }}}
   */
  def foldSmash[A](prefix: A, delim: A, suffix: A)(implicit A: Monoid[A], F: Foldable[F]): A =
    A.combine(prefix, A.combine(F.intercalate(fa, delim), suffix))

  
  
   /**
   * Make a string using `Show`, named as `mkString_` to avoid conflict
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val l: List[Int] = List(1, 2, 3)
   * scala> l.mkString_("List(", ",", ")")
   * res0: String = List(1,2,3)
   * }}}
   */
  def mkString_[A: Monoid](prefix: String, delim: String, suffix: String)(implicit F: Functor[F], S: Show[A], ev1: Monoid[String], ev2: Foldable[F]): String =
    foldSmash(F.map(fa)(S.show))(prefix, delim, suffix)
}
