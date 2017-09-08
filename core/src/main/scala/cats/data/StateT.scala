package cats
package data

import cats.syntax.either._

/**
 * `StateT[F, S, A]` is similar to `Kleisli[F, S, A]` in that it takes an `S`
 * argument and produces an `A` value wrapped in `F`. However, it also produces
 * an `S` value representing the updated state (which is wrapped in the `F`
 * context along with the `A` value.
 */
final class StateT[F[_], S, A](val runF: F[S => F[(S, A)]]) extends Serializable {

  def flatMap[B](fas: A => StateT[F, S, B])(implicit F: FlatMap[F]): StateT[F, S, B] =
    StateT.applyF(F.map(runF) { sfsa =>
      sfsa.andThen { fsa =>
        F.flatMap(fsa) { case (s, a) =>
          fas(a).run(s)
        }
      }
    })

  def flatMapF[B](faf: A => F[B])(implicit F: FlatMap[F]): StateT[F, S, B] =
    StateT.applyF(F.map(runF) { sfsa =>
      sfsa.andThen { fsa =>
        F.flatMap(fsa) { case (s, a) => F.map(faf(a))((s, _)) }
      }
    })

  def map[B](f: A => B)(implicit F: Functor[F]): StateT[F, S, B] =
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
   * value (discarding the final state).
   */
  def runEmptyA(implicit S: Monoid[S], F: FlatMap[F]): F[A] = runA(S.empty)

  /**
   * Like [[map]], but also allows the state (`S`) value to be modified.
   */
  def transform[B](f: (S, A) => (S, B))(implicit F: Functor[F]): StateT[F, S, B] =
    StateT.applyF(
      F.map(runF) { sfsa =>
        sfsa.andThen { fsa =>
          F.map(fsa) { case (s, a) => f(s, a) }
        }
      })

  /**
   * Like [[transform]], but allows the context to change from `F` to `G`.
   */
  def transformF[G[_], B](f: F[(S, A)] => G[(S, B)])(implicit F: FlatMap[F], G: Applicative[G]): StateT[G, S, B] =
    StateT(s => f(run(s)))

  /**
   * Transform the state used.
   *
   * This is useful when you are working with many focused `StateT`s and want to pass in a
   * global state containing the various states needed for each individual `StateT`.
   *
   * {{{
   * scala> import cats.implicits._ // needed for StateT.apply
   * scala> type GlobalEnv = (Int, String)
   * scala> val x: StateT[Option, Int, Double] = StateT((x: Int) => Option((x + 1, x.toDouble)))
   * scala> val xt: StateT[Option, GlobalEnv, Double] = x.transformS[GlobalEnv](_._1, (t, i) => (i, t._2))
   * scala> val input = 5
   * scala> x.run(input)
   * res0: Option[(Int, Double)] = Some((6,5.0))
   * scala> xt.run((input, "hello"))
   * res1: Option[(GlobalEnv, Double)] = Some(((6,hello),5.0))
   * }}}
   */
  def transformS[R](f: R => S, g: (R, S) => R)(implicit F: Functor[F]): StateT[F, R, A] =
    StateT.applyF(F.map(runF) { sfsa =>
      { r: R =>
        val s = f(r)
        val fsa = sfsa(s)
        F.map(fsa) { case (s, a) => (g(r, s), a) }
      }
    })

  /**
   * Modify the state (`S`) component.
   */
  def modify(f: S => S)(implicit F: Functor[F]): StateT[F, S, A] =
    transform((s, a) => (f(s), a))

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[B](f: S => B)(implicit F: Functor[F]): StateT[F, S, B] =
    transform((s, _) => (s, f(s)))

  /**
   * Get the input state, without modifying the state.
   */
  def get(implicit F: Functor[F]): StateT[F, S, S] =
    inspect(identity)
}

object StateT extends StateTInstances {
  def apply[F[_], S, A](f: S => F[(S, A)])(implicit F: Applicative[F]): StateT[F, S, A] =
    new StateT(F.pure(f))

  def applyF[F[_], S, A](runF: F[S => F[(S, A)]]): StateT[F, S, A] =
    new StateT(runF)

  def pure[F[_], S, A](a: A)(implicit F: Applicative[F]): StateT[F, S, A] =
    StateT(s => F.pure((s, a)))

  def lift[F[_], S, A](fa: F[A])(implicit F: Applicative[F]): StateT[F, S, A] =
    StateT(s => F.map(fa)(a => (s, a)))

  def inspect[F[_], S, A](f: S => A)(implicit F: Applicative[F]): StateT[F, S, A] =
    StateT(s => F.pure((s, f(s))))

  def inspectF[F[_], S, A](f: S => F[A])(implicit F: Applicative[F]): StateT[F, S, A] =
    StateT(s => F.map(f(s))(a => (s, a)))

  def modify[F[_], S](f: S => S)(implicit F: Applicative[F]): StateT[F, S, Unit] =
    StateT(s => F.pure((f(s), ())))

  def modifyF[F[_], S](f: S => F[S])(implicit F: Applicative[F]): StateT[F, S, Unit] =
    StateT(s => F.map(f(s))(s => (s, ())))

  def get[F[_], S](implicit F: Applicative[F]): StateT[F, S, S] =
    StateT(s => F.pure((s, s)))

  def set[F[_], S](s: S)(implicit F: Applicative[F]): StateT[F, S, Unit] =
    StateT(_ => F.pure((s, ())))

  def setF[F[_], S](fs: F[S])(implicit F: Applicative[F]): StateT[F, S, Unit] =
    StateT(_ => F.map(fs)(s => (s, ())))
}

private[data] sealed trait StateTInstances extends StateTInstances1 {
  implicit def catsDataAlternativeForStateT[F[_], S](implicit FM: Monad[F], FA: Alternative[F]): Alternative[StateT[F, S, ?]] =
    new StateTAlternative[F, S] { implicit def F = FM; implicit def G = FA }
}

private[data] sealed trait StateTInstances1 extends StateTInstances2 {
  implicit def catsDataMonadErrorForStateT[F[_], S, E](implicit F0: MonadError[F, E]): MonadError[StateT[F, S, ?], E] =
    new StateTMonadError[F, S, E] { implicit def F = F0 }

  implicit def catsDataSemigroupKForStateT[F[_], S](implicit F0: Monad[F], G0: SemigroupK[F]): SemigroupK[StateT[F, S, ?]] =
    new StateTSemigroupK[F, S] { implicit def F = F0; implicit def G = G0 }
}

private[data] sealed trait StateTInstances2 extends StateTInstances3 {
  implicit def catsDataMonadForStateT[F[_], S](implicit F0: Monad[F]): Monad[StateT[F, S, ?]] =
    new StateTMonad[F, S] { implicit def F = F0 }
}

private[data] sealed trait StateTInstances3 {
  implicit def catsDataFunctorForStateT[F[_], S](implicit F0: Functor[F]): Functor[StateT[F, S, ?]] =
    new StateTFunctor[F, S] { implicit def F = F0 }
}

// To workaround SI-7139 `object State` needs to be defined inside the package object
// together with the type alias.
private[data] abstract class StateFunctions {

  def apply[S, A](f: S => (S, A)): State[S, A] =
    StateT.applyF(Now((s: S) => Now(f(s))))

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

private[data] sealed trait StateTFunctor[F[_], S] extends Functor[StateT[F, S, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: StateT[F, S, A])(f: A => B): StateT[F, S, B] = fa.map(f)
}

private[data] sealed trait StateTMonad[F[_], S] extends Monad[StateT[F, S, ?]] with StateTFunctor[F, S] {
  implicit def F: Monad[F]

  def pure[A](a: A): StateT[F, S, A] =
    StateT.pure(a)

  def flatMap[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(f: A => StateT[F, S, Either[A, B]]): StateT[F, S, B] =
    StateT[F, S, B](s => F.tailRecM[(S, A), (S, B)]((s, a)) {
      case (s, a) => F.map(f(a).run(s)) { case (s, ab) => ab.bimap((s, _), (s, _)) }
    })
}

private[data] sealed trait StateTSemigroupK[F[_], S] extends SemigroupK[StateT[F, S, ?]] {
  implicit def F: Monad[F]
  implicit def G: SemigroupK[F]

  def combineK[A](x: StateT[F, S, A], y: StateT[F, S, A]): StateT[F, S, A] =
    StateT(s => G.combineK(x.run(s), y.run(s)))
}

private[data] sealed trait StateTAlternative[F[_], S] extends Alternative[StateT[F, S, ?]] with StateTFunctor[F, S] {
  implicit def F: Monad[F]
  def G: Alternative[F]

  def combineK[A](x: StateT[F, S, A], y: StateT[F, S, A]): StateT[F, S, A] =
    StateT[F, S, A](s => G.combineK(x.run(s), y.run(s)))(G)

  def pure[A](a: A): StateT[F, S, A] =
    StateT.pure[F, S, A](a)(G)

  def empty[A]: StateT[F, S, A] =
    StateT.lift[F, S, A](G.empty[A])(G)

  override def ap[A, B](ff: StateT[F, S, A => B])(fa: StateT[F, S, A]): StateT[F, S, B] =
    StateT[F, S, B]((s: S) =>
      F.flatMap(ff.run(s)) { sab =>
        val (sn, f) = sab
        F.map(fa.run(sn)) { case (snn, a) => (snn, f(a)) }
      }
    )
}

private[data] sealed trait StateTMonadError[F[_], S, E] extends StateTMonad[F, S] with MonadError[StateT[F, S, ?], E] {
  implicit def F: MonadError[F, E]

  def raiseError[A](e: E): StateT[F, S, A] = StateT.lift(F.raiseError(e))

  def handleErrorWith[A](fa: StateT[F, S, A])(f: E => StateT[F, S, A]): StateT[F, S, A] =
    StateT(s => F.handleErrorWith(fa.run(s))(e => f(e).run(s)))
}
