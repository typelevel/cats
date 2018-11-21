package cats
package laws

/**
 * Laws that must be obeyed by any `cats.InvariantAddMonoidal`.
 */
trait InvariantAddMonoidalLaws[F[_]] extends InvariantAddSemigroupalLaws[F] {
  implicit override def I: InvariantAddMonoidal[F]
  import cats.syntax.invariant._

  def sumEmptyLeftIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    I.sum(I.zero, fa).imap(_.right.get)(Right(_)) <-> fa

  def sumEmptyRightIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    I.sum(fa, I.zero).imap(_.left.get)(Left(_)) <-> fa
}

object InvariantAddMonoidalLaws {
  def apply[F[_]](implicit i: InvariantAddMonoidal[F]): InvariantAddMonoidalLaws[F] =
    new InvariantAddMonoidalLaws[F] { def I: InvariantAddMonoidal[F] = i }
}
