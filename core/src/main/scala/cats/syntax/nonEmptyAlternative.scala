package cats
package syntax

trait NonEmptyAlternativeSyntax {
  implicit final def catsSyntaxNonEmptyAlternative[F[_], A](fa: F[A]): NonEmptyAlternativeOps[F, A] =
    new NonEmptyAlternativeOps(fa)
}

final class NonEmptyAlternativeOps[F[_], A] private[syntax] (private val fa: F[A]) extends AnyVal {

  /**
   * @see [[NonEmptyAlternative.prependK]]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> List(2, 3, 4).prependK(1)
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def prependK(a: A)(implicit F: NonEmptyAlternative[F]): F[A] = F.prependK(a, fa)

  /**
   * @see [[NonEmptyAlternative.appendK]]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> List(1, 2, 3).appendK(4)
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def appendK(a: A)(implicit F: NonEmptyAlternative[F]): F[A] = F.appendK(fa, a)
}
