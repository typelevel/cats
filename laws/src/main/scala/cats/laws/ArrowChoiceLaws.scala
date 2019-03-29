package cats
package laws

import cats.arrow.ArrowChoice
import cats.syntax.arrowChoice._
import cats.syntax.compose._
import cats.syntax.profunctor._

/**
 * Laws that must be obeyed by any `cats.arrow.ArrowChoice`.
 */
trait ArrowChoiceLaws[F[_, _]] extends ArrowLaws[F] with ChoiceLaws[F] {
  implicit override def F: ArrowChoice[F]
  implicit def Function: ArrowChoice[Function1]

  def sumAssoc[A, B, C](e: Either[Either[A, B], C]): Either[A, Either[B, C]] =
    e match {
      case Left(Left(x))  => Left(x)
      case Left(Right(y)) => Right(Left(y))
      case Right(z)       => Right(Right(z))
    }

  def leftLiftCommute[A, B, C](f: A => B): IsEq[F[Either[A, C], Either[B, C]]] =
    F.left[A, B, C](F.lift[A, B](f)) <-> F.lift[Either[A, C], Either[B, C]](Function.left[A, B, C](f))

  def leftComposeCommute[A, B, C, D](f: F[A, B], g: F[B, C]): IsEq[F[Either[A, D], Either[C, D]]] =
    F.left(f >>> g) <-> (F.left(f) >>> F.left[B, C, D](g))

  def leftRightConsistent[A, B, C](f: A => B): IsEq[F[Either[C, A], Either[C, B]]] =
    F.right[A, B, C](F.lift[A, B](f)) <->
      F.left[A, B, C](F.lift[A, B](f)).dimap((x: Either[C, A]) => x.swap)((y: Either[B, C]) => y.swap)

  def leftAndThenLiftedLeftApplyCommutes[A, B, C](f: F[A, B]): IsEq[F[A, Either[B, C]]] =
    (f >>> F.lift[B, Either[B, C]](Left.apply[B, C])) <-> (F.lift[A, Either[A, C]](Left.apply[A, C] _) >>> F.left(f))

  def leftAndThenRightIdentityCommutes[A, B, C, D](f: F[A, B], g: C => D): IsEq[F[Either[A, C], Either[B, D]]] =
    (F.left(f) >>> F.lift(identity[B] _ +++ g)) <-> (F.lift(identity[A] _ +++ g) >>> F.left(f))

  def leftTwiceCommutesWithSumAssociation[A, B, C, D](
    f: F[A, D]
  ): IsEq[F[Either[Either[A, B], C], Either[D, Either[B, C]]]] =
    (F.left(F.left[A, D, B](f)) >>> F.lift(sumAssoc[D, B, C])) <->
      (F.lift(sumAssoc[A, B, C]) >>> F.left(f))
}

object ArrowChoiceLaws {
  def apply[F[_, _]](implicit ev: ArrowChoice[F], f: ArrowChoice[Function1]): ArrowChoiceLaws[F] =
    new ArrowChoiceLaws[F] {
      def F: ArrowChoice[F] = ev
      def Function: ArrowChoice[Function1] = f
    }
}
