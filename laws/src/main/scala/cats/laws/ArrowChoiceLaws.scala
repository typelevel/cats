package cats
package laws

import cats.arrow.ArrowChoice
import cats.syntax.compose._

/**
 * Laws that must be obeyed by any `cats.arrow.ArrowChoice`.
 */
trait ArrowChoiceLaws[F[_, _]] extends ArrowLaws[F] with ChoiceLaws[F] {
  implicit override def F: ArrowChoice[F]
  implicit def Function: ArrowChoice[Function1]

  def leftLiftCommute[A, B, C](f: A => B): IsEq[F[Either[A, C], Either[B, C]]] =
    F.left[A, B, C](F.lift[A, B](f)) <-> F.lift[Either[A, C], Either[B, C]](Function.left[A, B, C](f))

  def rightLiftCommute[A, B, C](f: A => B): IsEq[F[Either[C, A], Either[C, B]]] =
    F.right[A, B, C](F.lift[A, B](f)) <-> F.lift[Either[C, A], Either[C, B]](Function.right[A, B, C](f))

  def chooseLiftCommute[A, B, C, D](f: A => C, g: B => D): IsEq[F[Either[A, B], Either[C, D]]] =
    F.lift[Either[A, B], Either[C, D]](Function.choose(f)(g)) <-> F.choose[A, B, C, D](F.lift(f))(F.lift(g))

  def choiceLiftCommute[A, B, C](f: A => C, g: B => C): IsEq[F[Either[A, B], C]] =
    F.lift[Either[A, B], C](Function.choice[A, B, C](f, g)) <-> F.choice[A, B, C](F.lift[A, C](f), F.lift[B, C](g))

  def leftComposeCommute[A, B, C, D](f: A => B, g: B => C): IsEq[F[Either[A, D], Either[C, D]]] =
    F.left[A, C, D](F.lift(g compose f)) <->
      (F.lift[Either[B, D], Either[C, D]](Function.left(g)) <<<
      F.lift[Either[A, D], Either[B, D]](Function.left(f)))

  def rightComposeCommute[A, B, C, D](f: A => B, g: B => C): IsEq[F[Either[D, A], Either[D, C]]] =
    F.right(F.lift(g compose f)) <->
      (F.lift[Either[D, B], Either[D, C]](Function.right(g)) <<<
      F.lift[Either[D, A], Either[D, B]](Function.right(f)))
}

object ArrowChoiceLaws {
  def apply[F[_, _]](implicit ev: ArrowChoice[F], f: ArrowChoice[Function1]): ArrowChoiceLaws[F] =
    new ArrowChoiceLaws[F] {
      def F: ArrowChoice[F] = ev
      def Function: ArrowChoice[Function1] = f
    }
}
