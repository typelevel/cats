package cats
package state

import cats.free.Trampoline
import cats.data.Kleisli
import cats.std.function.function0Instance

/**
 * `StateT[F, S, A]` is similar to `Kleisli[F, S, A]` in that it takes an `S`
 * argument and produces an `A` value wrapped in `F`. However, it also produces
 * an `S` value representing the updated state (which is wrapped in the `F`
 * context along with the `A` value.
 */
final class StateT[F[_], S, A](val runF: F[S => F[(S, A)]]) extends Serializable {

  def flatMap[B](fas: A => StateT[F, S, B])(implicit F: Monad[F]): StateT[F, S, B] =
    StateT(s =>
      F.flatMap(runF) { fsf =>
        F.flatMap(fsf(s)) { case (s, a) =>
          fas(a).run(s)
        }
      })

  def map[B](f: A => B)(implicit F: Monad[F]): StateT[F, S, B] =
    transform { case (s, a) => (s, f(a)) }

  /**
   * Run with the provided initial state value
   */
  def run(initial: S)(implicit F: FlatMap[F]): F[(S, A)] =
    F.flatMap(runF)(f => f(initial))

  /**
   * Run with the provided initial state value and return the final state
   * (discarding the final value).
   */
  def runS(s: S)(implicit F: FlatMap[F]): F[S] = F.map(run(s))(_._1)

  /**
   * Run with the provided initial state value and return the final value
   * (discarding the final state).
   */
  def runA(s: S)(implicit F: FlatMap[F]): F[A] = F.map(run(s))(_._2)

  /**
   * Run with `S`'s empty monoid value as the initial state.
   */
  def runEmpty(implicit S: Monoid[S], F: FlatMap[F]): F[(S, A)] = run(S.empty)

  /**
   * Run with `S`'s empty monoid value as the initial state and return the final
   * state (discarding the final value).
   */
  def runEmptyS(implicit S: Monoid[S], F: FlatMap[F]): F[S] = runS(S.empty)

  /**
   * Run with `S`'s empty monoid value as the initial state and return the final
   * state (discarding the final value).
   */
  def runEmptyA(implicit S: Monoid[S], F: FlatMap[F]): F[A] = runA(S.empty)

  /**
   * Like [[map]], but also allows the state (`S`) value to be modified.
   */
  def transform[B](f: (S, A) => (S, B))(implicit F: Monad[F]): StateT[F, S, B] =
    transformF { fsa =>
      F.map(fsa){ case (s, a) => f(s, a) }
    }

  /**
   * Like [[transform]], but allows the context to change from `F` to `G`.
   */
  def transformF[G[_], B](f: F[(S, A)] => G[(S, B)])(implicit F: FlatMap[F], G: Applicative[G]): StateT[G, S, B] =
    StateT(s => f(run(s)))

  /**
   * Modify the state (`S`) component.
   */
  def modify(f: S => S)(implicit F: Monad[F]): StateT[F, S, A] =
    transform((s, a) => (f(s), a))

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[B](f: S => B)(implicit F: Monad[F]): StateT[F, S, B] =
    transform((s, _) => (s, f(s)))

}

object StateT extends StateTInstances {
  def apply[F[_], S, A](f: S => F[(S, A)])(implicit F: Applicative[F]): StateT[F, S, A] =
    new StateT(F.pure(f))

  def applyF[F[_], S, A](runF: F[S => F[(S, A)]]): StateT[F, S, A] =
    new StateT(runF)

  def pure[F[_], S, A](a: A)(implicit F: Applicative[F]): StateT[F, S, A] =
    StateT(s => F.pure((s, a)))
}

sealed abstract class StateTInstances extends StateTInstances0 {
  implicit def stateTMonadState[F[_], S](implicit F: Monad[F]): MonadState[StateT[F, ?, ?], S] =
    new MonadState[StateT[F, ?, ?], S] {
      def pure[A](a: A): StateT[F, S, A] =
        StateT.pure(a)

      def flatMap[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] =
        fa.flatMap(f)

      val get: StateT[F, S, S] = StateT(a => F.pure((a, a)))

      def set(s: S): StateT[F, S, Unit] = StateT(_ => F.pure((s, ())))

      override def map[A, B](fa: StateT[F, S, A])(f: A => B): StateT[F, S, B] =
        fa.map(f)
    }
}

sealed abstract class StateTInstances0 {
  implicit def stateMonadState[S]: MonadState[State[?, ?], S] =
    StateT.stateTMonadState[Trampoline, S]
}

// To workaround SI-7139 `object State` needs to be defined inside the package object
// together with the type alias.
abstract class StateFunctions {

  def apply[S, A](f: S => (S, A)): State[S, A] =
    StateT.applyF(Trampoline.done((s: S) => Trampoline.done(f(s))))

  /**
   * Return `a` and maintain the input state.
   */
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))

  /**
   * Modify the input state and return Unit.
   */
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[S, T](f: S => T): State[S, T] = State(s => (s, f(s)))

  /**
   * Return the input state without modifying it.
   */
  def get[S]: State[S, S] = inspect(identity)

  /**
   * Set the state to `s` and return Unit.
   */
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
}
