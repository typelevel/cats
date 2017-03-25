package cats
package laws

import cats.syntax.all._
import cats.laws.MonadDeferLaws.StatefulBox

trait MonadDeferLaws[F[_]] extends MonadLaws[F] {
  implicit override def F: MonadDefer[F]

  def delayEquivalenceWithPure[A](a: A): IsEq[F[A]] =
    F.delay(a) <-> F.pure(a)

  def delayEquivalenceWithDefer[A, B](a: A, f: A => B): IsEq[F[B]] =
    F.delay(f(a)) <-> F.defer(F.pure(f(a)))

  def delayRepeatsSideEffects[A, B](a: A, b: B, f: (A, B) => A): IsEq[F[A]] = {
    val state = new StatefulBox(a)
    val fa = F.delay(state.transform(a => f(a, b)))
    fa.flatMap(_ => fa) <-> F.pure(f(f(a, b), b))
  }

  def deferRepeatsSideEffects[A, B](a: A, b: B, f: (A, B) => A): IsEq[F[A]] = {
    val state = new StatefulBox(a)
    val fa = F.defer(F.pure(state.transform(a => f(a, b))))
    fa.flatMap(_ => fa) <-> F.pure(f(f(a, b), b))
  }


  lazy val flatMapStackSafety: IsEq[F[Int]] = {
    // tailRecM expressed with flatMap
    def loop[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
      F.flatMap(f(a)) {
        case Right(b) =>
          F.pure(b)
        case Left(nextA) =>
          loop(nextA)(f)
      }

    val n = 50000
    val res = loop(0)(i => F.pure(if (i < n) Either.left(i + 1) else Either.right(i)))
    res <-> F.pure(n)
  }

  /** Optional law for `ApplicativeError`. */
  def delayCapturesExceptions[A](ex: Throwable)
    (implicit A: ApplicativeError[F, Throwable]): IsEq[F[A]] =
    F.delay[A](throw ex) <-> A.raiseError[A](ex)

  /** Optional law for `ApplicativeError`. */
  def deferCapturesExceptions[A](ex: Throwable)
    (implicit A: ApplicativeError[F, Throwable]): IsEq[F[A]] =
    F.defer[A](throw ex) <-> A.raiseError[A](ex)
}

object MonadDeferLaws {
  def apply[F[_]](implicit ev: MonadDefer[F]): MonadDeferLaws[F] =
    new MonadDeferLaws[F] { def F: MonadDefer[F] = ev }

  /**
   * A boxed and synchronized variable to use for
   * testing deferred side effects.
   */
  final class StatefulBox[A](initial: A) {
    private[this] var state = initial

    def get: A =
      synchronized(state)

    def transform(f: A => A): A =
      synchronized{ state = f(state); state }
  }
}
