package cats
package syntax

private[syntax] trait FlatMapSyntax1 {
  implicit def catsSyntaxUFlatMap[FA](fa: FA)(implicit U: Unapply[FlatMap, FA]): FlatMap.Ops[U.M, U.A] =
    new FlatMap.Ops[U.M, U.A]{
      val self = U.subst(fa)
      val typeClassInstance = U.TC
    }
}

trait FlatMapSyntax extends FlatMap.ToFlatMapOps with FlatMapSyntax1 {

  implicit def catsSyntaxFlatten[F[_]: FlatMap, A](ffa: F[F[A]]): FlattenOps[F, A] =
    new FlattenOps[F, A](ffa)

  implicit def catsSyntaxIfM[F[_]: FlatMap](fa: F[Boolean]): IfMOps[F] =
    new IfMOps[F](fa)
}

final class FlattenOps[F[_], A](ffa: F[F[A]])(implicit F: FlatMap[F]) {

  /**
   * Flatten nested `F` values.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> type ErrorOr[A] = Either[String, A]
   * scala> val x: ErrorOr[ErrorOr[Int]] = Right(Right(3))
   * scala> x.flatten
   * res0: ErrorOr[Int] = Right(3)
   * }}}
   */
  def flatten: F[A] = F.flatten(ffa)
}

final class IfMOps[F[_]](fa: F[Boolean])(implicit F: FlatMap[F]) {

  /**
   * A conditional lifted into the `F` context.
   *
   * Example:
   * {{{
   * scala> import cats.{Eval, Now}
   * scala> import cats.implicits._
   *
   * scala> val b1: Eval[Boolean] = Now(true)
   * scala> val asInt1: Eval[Int] = b1.ifM(Now(1), Now(0))
   * scala> asInt1.value
   * res0: Int = 1
   *
   * scala> val b2: Eval[Boolean] = Now(false)
   * scala> val asInt2: Eval[Int] = b2.ifM(Now(1), Now(0))
   * scala> asInt2.value
   * res1: Int = 0
   * }}}
   */
  def ifM[B](ifTrue: => F[B], ifFalse: => F[B]): F[B] = F.ifM(fa)(ifTrue, ifFalse)
}
