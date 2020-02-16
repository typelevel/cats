package cats
package syntax

trait SemigroupKSyntax extends SemigroupK.ToSemigroupKOps

private[syntax] trait SemigroupKSyntaxBinCompat0 {
  implicit final def catsSyntaxSemigroupKBinCompat0[F[_]](fa: SemigroupK[F]): SemigroupKOps[F] =
    new SemigroupKOps(fa)
}

final private[syntax] class SemigroupKOps[F[_]](val S: SemigroupK[F]) extends AnyVal {

  /**
   * Combines `F[A]` and `F[B]` into a `F[Either[A,B]]]`.
   *
   * Example:
   * {{{
   * scala> import cats.SemigroupK
   * scala> import cats.syntax.semigroupk._
   * scala> import cats.data.NonEmptyList
   * scala> SemigroupK[NonEmptyList].sum(NonEmptyList.one("abc"), NonEmptyList.one(2))
   * res0: NonEmptyList[Either[String,Int]] = NonEmptyList(Left(abc), Right(2))
   * }}}
   */
  def sum[A, B](fa: F[A], fb: F[B])(implicit F: Functor[F]): F[Either[A, B]] =
    S.combineK(F.map(fa)(Left(_)), F.map(fb)(Right(_)))

}
