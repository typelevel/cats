package cats
package syntax

import cats.data.XorT

trait XorTSyntax extends XorTSyntax1 {
  implicit def catsSyntaxXorT[F[_]: Functor, A](fa: F[A]): XorTFunctorOps[F, A] = new XorTFunctorOps(fa)
}

private[syntax] trait XorTSyntax1 {
  implicit def catsSyntaxUXorT[FA](fa: FA)(implicit U: Unapply[Functor, FA]): XorTFunctorOps[U.M, U.A] =
    new XorTFunctorOps[U.M, U.A](U.subst(fa))(U.TC)
}

final class XorTFunctorOps[F[_]: Functor, A](val fa: F[A]) {
  /**
   * Lift this `F[A]` into a `XorT[F, A, R]`.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> val oa: Option[String] = Some("boo")
   * scala> oa.leftXorT[Int]
   * res0: cats.data.XorT[Option,String,Int] = XorT(Some(Left(boo)))
   * }}}
   */
  def leftXorT[R]: XorT[F, A, R] = XorT.left(fa)

  /**
   * Lift this `F[A]` into a `XorT[F, L, A]`.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> val oa: Option[Int] = Some(1)
   * scala> oa.rightXorT[String]
   * res0: cats.data.XorT[Option,String,Int] = XorT(Some(Right(1)))
   * }}}
   */
  def rightXorT[L]: XorT[F, L, A] = XorT.right(fa)
}
