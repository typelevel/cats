package cats
package laws

import cats.syntax.all._
import cats.laws.MonadDeferLaws.StatefulBox

trait MonadDeferLaws[F[_]] extends ApplicativeEvalLaws[F] with MonadLaws[F] {
  implicit override def F: MonadDefer[F]

  def evalSequenceConsistentWithPureMap[A, B](a: A, b: B, f: (A, B) => A): IsEq[F[A]] = {
    def tr(s: StatefulBox[A]) = s.transform(a => f(a, b))
    val state1 = new StatefulBox(a)
    val state2 = new StatefulBox(a)

    val lh = F.delay(tr(state1))
    val rh = F.pure(state2).map(tr)

    lh.flatMap(_ => lh) <-> rh.flatMap(_ => rh)
  }

  def deferSequenceConsistentWithPureFlatMap[A, B](a: A, b: B, f: (A, B) => A): IsEq[F[A]] = {
    def tr(s: StatefulBox[A]) = F.pure(s.transform(a => f(a, b)))
    val state1 = new StatefulBox(a)
    val state2 = new StatefulBox(a)

    val lh = F.defer(tr(state1))
    val rh = F.pure(state2).flatMap(tr)

    lh.flatMap(_ => lh) <-> rh.flatMap(_ => rh)
  }

  def evalRepeatsSideEffects[A,B](a: A, b: B, f: (A, B) => A): IsEq[F[A]] = {
    val state = new StatefulBox(a)
    val fa = F.delay(state.transform(a => f(a, b)))
    fa.flatMap(_ => fa) <-> F.pure(f(f(a,b), b))
  }

  def deferRepeatsSideEffects[A,B](a: A, b: B, f: (A, B) => A): IsEq[F[A]] = {
    val state = new StatefulBox(a)
    val fa = F.defer(F.pure(state.transform(a => f(a,b))))
    fa.flatMap(_ => fa) <-> F.pure(f(f(a,b), b))
  }
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