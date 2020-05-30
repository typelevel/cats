package cats
package syntax

trait AlternativeSyntax {
  // TODO: use simulacrum instances eventually
  implicit final def catsSyntaxUnite[F[_], G[_], A](fga: F[G[A]]): UniteOps[F, G, A] =
    new UniteOps[F, G, A](fga)

  implicit final def catsSyntaxAlternativeSeparate[F[_], G[_, _], A, B](fgab: F[G[A, B]]): SeparateOps[F, G, A, B] =
    new SeparateOps[F, G, A, B](fgab)

  implicit final def catsSyntaxAlternativeGuard(b: Boolean): GuardOps =
    new GuardOps(b)
}

final class UniteOps[F[_], G[_], A](private val fga: F[G[A]]) extends AnyVal {

  /**
   * @see [[Alternative.unite]]
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val x: List[Vector[Int]] = List(Vector(1, 2), Vector(3, 4))
   * scala> x.unite
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def unite(implicit F: Monad[F], A: Alternative[F], G: Foldable[G]): F[A] = A.unite[G, A](fga)
}

final class SeparateOps[F[_], G[_, _], A, B](private val fgab: F[G[A, B]]) extends AnyVal {

  /**
   * @see [[Alternative.separate]]
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Either[String, Int]] = List(Right(1), Left("error"))
   * scala> l.separate
   * res0: (List[String], List[Int]) = (List(error),List(1))
   * }}}
   */
  def separate(implicit F: Monad[F], A: Alternative[F], G: Bifoldable[G]): (F[A], F[B]) = A.separate[G, A, B](fgab)

  /**
   * @see [[Alternative.separateFoldable]]
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val l: List[Either[String, Int]] = List(Right(1), Left("error"))
   * scala> l.separateFoldable
   * res0: (List[String], List[Int]) = (List(error),List(1))
   * }}}
   */
  def separateFoldable(implicit F: Foldable[F], A: Alternative[F], G: Bifoldable[G]): (F[A], F[B]) =
    A.separateFoldable[G, A, B](fgab)
}

final class GuardOps(private val condition: Boolean) extends AnyVal {

  /**
   * @see [[Alternative.guard]]
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> def even(i: Int): Option[String] = (i % 2 == 0).guard[Option].as("even")
   * scala> even(2)
   * res0: Option[String] = Some(even)
   * scala> even(3)
   * res1: Option[String] = None
   * }}}
   */
  def guard[F[_]](implicit F: Alternative[F]): F[Unit] = F.guard(condition)
}
