package cats
package laws

/**
 * Laws that must be obeyed by any `cats.InvariantChoosable`.
 */
trait InvariantChoosableLaws[F[_]] extends InvariantChoiceLaws[F] {
  implicit override def I: InvariantChoosable[F]
  import cats.syntax.invariant._

  def choiceZeroLeftIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    I.choice[Nothing, A](I.zero, fa).imap(leftNothing)(Right(_)) <-> fa

  def choiceZeroRightIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    I.choice[A, Nothing](fa, I.zero).imap(rightNothing)(Left(_)) <-> fa

  private def leftNothing[A](e: Either[Nothing, A]): A =
    e.fold(identity, identity)

  private def rightNothing[A](e: Either[A, Nothing]): A =
    e.fold(identity, identity)

}

object InvariantChoosableLaws {
  def apply[F[_]](implicit i: InvariantChoosable[F]): InvariantChoosableLaws[F] =
    new InvariantChoosableLaws[F] { def I: InvariantChoosable[F] = i }
}
