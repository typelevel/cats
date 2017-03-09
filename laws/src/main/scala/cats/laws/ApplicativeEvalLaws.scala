package cats
package laws

import cats.Eval.{always, now}
import cats.syntax.all._

trait ApplicativeEvalLaws[F[_]] extends ApplicativeLaws[F] {
  implicit override def F: ApplicativeEval[F]

  def evalEquivalenceWithPure[A](a: A): IsEq[F[A]] =
    F.eval(now(a)) <-> F.pure(a)

  def evalConsistentWithPureMapped[A, B](a: A, f: A => B): IsEq[F[B]] =
    F.eval(always(f(a))) <-> F.pure(a).map(f)

  def evalCapturesExceptions[A](ex: Throwable)
    (implicit A: ApplicativeError[F, Throwable]): IsEq[F[A]] =
    F.eval[A](always(throw ex)) <-> A.raiseError[A](ex)
}

object ApplicativeEvalLaws {
  def apply[F[_]](implicit ev: ApplicativeEval[F]): ApplicativeEvalLaws[F] =
    new ApplicativeEvalLaws[F] { def F: ApplicativeEval[F] = ev }
}
