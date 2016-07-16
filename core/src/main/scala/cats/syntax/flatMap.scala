package cats
package syntax

private[syntax] trait FlatMapSyntax1 {
  implicit def catsSyntaxUFlatMap[FA](fa: FA)(implicit U: Unapply[FlatMap, FA]): FlatMapOps[U.M, U.A] =
    new FlatMapOps[U.M, U.A](U.subst(fa))(U.TC)
}

trait FlatMapSyntax extends FlatMapSyntax1 {
  implicit def catsSyntaxFlatMap[F[_]: FlatMap, A](fa: F[A]): FlatMapOps[F, A] =
    new FlatMapOps(fa)

  implicit def catsSyntaxFlatten[F[_]: FlatMap, A](ffa: F[F[A]]): FlattenOps[F, A] =
    new FlattenOps[F, A](ffa)

  implicit def catsSyntaxIfM[F[_]: FlatMap](fa: F[Boolean]): IfMOps[F] =
    new IfMOps[F](fa)
}

final class FlatMapOps[F[_], A](fa: F[A])(implicit F: FlatMap[F]) {
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)

  /**
   * Pair `A` with the result of function application.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> List("12", "34", "56").mproduct(_.toList)
   * res0: List[(String, Char)] = List((12,1), (12,2), (34,3), (34,4), (56,5), (56,6))
   * }}}
   */
  def mproduct[B](f: A => F[B]): F[(A, B)] = F.mproduct(fa)(f)

  def >>=[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)

  /** Alias for [[followedBy]]. */
  @inline final def >>[B](fb: F[B]): F[B] = followedBy(fb)

  /** Sequentially compose two actions, discarding any value produced by the first. */
  def followedBy[B](fb: F[B]): F[B] = F.flatMap(fa)(_ => fb)

  /**
   * Sequentially compose two actions, discarding any value produced by the first. This variant of
   * [[followedBy]] also lets you define the evaluation strategy of the second action. For instance
   * you can evaluate it only ''after'' the first action has finished:
   *
   * {{{
   * scala> import cats.Eval
   * scala> import cats.implicits._
   * scala> val fa: Option[Int] = Some(3)
   * scala> def fb: Option[String] = Some("foo")
   * scala> fa.followedByEval(Eval.later(fb))
   * res0: Option[String] = Some(foo)
   * }}}
   */
  def followedByEval[B](fb: Eval[F[B]]): F[B] = F.flatMap(fa)(_ => fb.value)

}

final class FlattenOps[F[_], A](ffa: F[F[A]])(implicit F: FlatMap[F]) {

  /**
   * Flatten nested `F` values.
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.implicits._
   * scala> type ErrorOr[A] = String Xor A
   * scala> val x: ErrorOr[ErrorOr[Int]] = Xor.right(Xor.right(3))
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
