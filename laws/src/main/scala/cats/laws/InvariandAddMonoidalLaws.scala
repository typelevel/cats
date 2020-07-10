package cats
package laws

import cats.data.INothing

/**
 * Laws that must be obeyed by any `cats.InvariantChoosable`.
 */
trait InvariantChoosableLaws[F[_]] extends InvariantChoiceLaws[F] {
  implicit override def I: InvariantChoosable[F]
  import cats.syntax.invariant._

  def choiceEmptyLeftIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    I.choice(I.zero, fa).imap(leftNothing)(Right(_)) <-> fa

  def sumEmptyRightIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    I.choice(fa, I.zero).imap(rightNothing)(Left(_)) <-> fa

  private def leftNothing[A](e: Either[INothing, A]): A =
    e.fold(INothing.absurd, identity)

  private def rightNothing[A](e: Either[A, INothing]): A =
    e.fold(identity, INothing.absurd)

}

object InvariantChoosableLaws {
  def apply[F[_]](implicit i: InvariantChoosable[F]): InvariantChoosableLaws[F] =
    new InvariantChoosableLaws[F] { def I: InvariantChoosable[F] = i }
}
