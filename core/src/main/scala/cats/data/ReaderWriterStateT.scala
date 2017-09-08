package cats
package data

import cats.functor.{ Contravariant, Profunctor }
import cats.syntax.either._

/**
 * Represents a stateful computation in a context `F[_]`, over state `S`, with an initial environment `E`,
 * an accumulated log `L` and a result `A`.
 *
 * In other words, it is a pre-baked stack of `[[ReaderT]][F, E, A]`, `[[WriterT]][F, L, A]`
 * and `[[StateT]][F, S, A]`.
 */
final class ReaderWriterStateT[F[_], E, L, S, A](val runF: F[(E, S) => F[(L, S, A)]]) extends Serializable {

  /**
   * Modify the initial environment using `f`.
   */
  def contramap[E0](f: E0 => E)(implicit F: Functor[F]): ReaderWriterStateT[F, E0, L, S, A] =
    ReaderWriterStateT.applyF {
      F.map(runF) { rwsa =>
        (e0: E0, s: S) => rwsa(f(e0), s)
      }
    }

  /**
   * Alias for [[contramap]].
   */
  def local[EE](f: EE => E)(implicit F: Functor[F]): ReaderWriterStateT[F, EE, L, S, A] =
    contramap(f)

  /**
   * Modify the result of the computation using `f`.
   */
  def map[B](f: A => B)(implicit F: Functor[F]): ReaderWriterStateT[F, E, L, S, B] =
    transform { (l, s, a) => (l, s, f(a)) }

  /**
   * Modify the written log value using `f`.
   */
  def mapWritten[LL](f: L => LL)(implicit F: Functor[F]): ReaderWriterStateT[F, E, LL, S, A] =
    transform { (l, s, a) => (f(l), s, a) }

  /**
   * Modify the result of the computation by feeding it into `f`, threading the state
   * through the resulting computation and combining the log values.
   */
  def flatMap[B](f: A => ReaderWriterStateT[F, E, L, S, B])(
    implicit F: FlatMap[F], L: Semigroup[L]): ReaderWriterStateT[F, E, L, S, B] =
    ReaderWriterStateT.applyF {
      F.map(runF) { rwsfa =>
        (e: E, s0: S) =>
          F.flatMap(rwsfa(e, s0)) { case (la, sa, a) =>
            F.flatMap(f(a).runF) { rwsfb =>
              F.map(rwsfb(e, sa)) { case (lb, sb, b) =>
                (L.combine(la, lb), sb, b)
              }
            }
          }
      }
    }

  /**
   * Like [[map]], but allows the mapping function to return an effectful value.
   */
  def flatMapF[B](faf: A => F[B])(implicit F: FlatMap[F]): ReaderWriterStateT[F, E, L, S, B] =
    ReaderWriterStateT.applyF {
      F.map(runF) { rwsfa =>
        (e: E, s: S) =>
          F.flatMap(rwsfa(e, s)) { case (l, s, a) =>
            F.map(faf(a))((l, s, _))
          }
      }
    }

  /**
   * Transform the resulting log, state and value using `f`.
   */
  def transform[LL, B](f: (L, S, A) => (LL, S, B))(implicit F: Functor[F]): ReaderWriterStateT[F, E, LL, S, B] =
    ReaderWriterStateT.applyF {
      F.map(runF) { rwsfa =>
        (e: E, s: S) => F.map(rwsfa(e, s)) { case (l, s, a) =>
          val (ll, sb, b) = f(l, s, a)
          (ll, sb, b)
        }
      }
    }

  /**
   * Like [[transform]], but allows the context to change from `F` to `G`.
   */
  def transformF[G[_], LL, B](f: F[(L, S, A)] => G[(LL, S, B)])(
    implicit F: Monad[F], G: Applicative[G]): ReaderWriterStateT[G, E, LL, S, B] =
    ReaderWriterStateT.apply((e, s) => f(run(e, s)))


  /**
   * Modify the resulting state.
   */
  def modify(f: S => S)(implicit F: Functor[F]): ReaderWriterStateT[F, E, L, S, A] =
    transform { (l, s, a) => (l, f(s), a) }

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[B](f: S => B)(implicit F: Functor[F]): ReaderWriterStateT[F, E, L, S, B] =
    transform { (l, s, a) => (l, s, f(s)) }

  /**
   * Get the input state, without modifying it.
   */
  def get(implicit F: Functor[F]): ReaderWriterStateT[F, E, L, S, S] =
    inspect(identity)

  /**
   * Add a value to the log.
   */
  def tell(l: L)(implicit F: Functor[F], L: Semigroup[L]): ReaderWriterStateT[F, E, L, S, A] =
    mapWritten(L.combine(_, l))

  /**
   * Retrieve the value written to the log.
   */
  def written(implicit F: Functor[F]): ReaderWriterStateT[F, E, L, S, L] =
    transform { (l, s, a) => (l, s, l) }

  /**
   * Clear the log.
   */
  def reset(implicit F: Functor[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, A] =
    mapWritten(_ => L.empty)

  /**
   * Run the computation using the provided initial environment and state.
   */
  def run(env: E, initial: S)(implicit F: Monad[F]): F[(L, S, A)] =
    F.flatMap(runF)(_.apply(env, initial))

  /**
   * Run the computation using the provided environment and an empty state.
   */
  def runEmpty(env: E)(implicit F: Monad[F], S: Monoid[S]): F[(L, S, A)] =
    run(env, S.empty)

  /**
   * Like [[run]], but discards the final state and log.
   */
  def runA(env: E, initial: S)(implicit F: Monad[F]): F[A] =
    F.map(run(env, initial))(_._3)

  /**
   * Like [[run]], but discards the final value and log.
   */
  def runS(env: E, initial: S)(implicit F: Monad[F]): F[S] =
    F.map(run(env, initial))(_._2)

  /**
   * Like [[run]], but discards the final state and value.
   */
  def runL(env: E, initial: S)(implicit F: Monad[F]): F[L] =
    F.map(run(env, initial))(_._1)

  /**
   * Like [[runEmpty]], but discards the final state and log.
   */
  def runEmptyA(env: E)(implicit F: Monad[F], S: Monoid[S]): F[A] =
    runA(env, S.empty)

  /**
   * Like [[runEmpty]], but discards the final value and log.
   */
  def runEmptyS(env: E)(implicit F: Monad[F], S: Monoid[S]): F[S] =
    runS(env, S.empty)

  /**
   * Like [[runEmpty]], but discards the final state and value.
   */
  def runEmptyL(env: E)(implicit F: Monad[F], S: Monoid[S]): F[L] =
    runL(env, S.empty)
}

object ReaderWriterStateT extends RWSTInstances {
  /**
   * Construct a new computation using the provided function.
   */
  def apply[F[_], E, L, S, A](runF: (E, S) => F[(L, S, A)])(implicit F: Applicative[F]): ReaderWriterStateT[F, E, L, S, A] =
    new ReaderWriterStateT(F.pure(runF))

  /**
   * Like [[apply]], but using a function in a context `F`.
   */
  def applyF[F[_], E, L, S, A](runF: F[(E, S) => F[(L, S, A)]]): ReaderWriterStateT[F, E, L, S, A] =
    new ReaderWriterStateT(runF)

  /**
   * Return `a` and an empty log without modifying the input state.
   */
  def pure[F[_], E, L, S, A](a: A)(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, A] =
    ReaderWriterStateT((_, s) => F.pure((L.empty, s, a)))

  /**
   * Return an effectful `a` and an empty log without modifying the input state.
   */
  def lift[F[_], E, L, S, A](fa: F[A])(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, A] =
    ReaderWriterStateT((_, s) => F.map(fa)((L.empty, s, _)))

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[F[_], E, L, S, A](f: S => A)(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, A] =
    ReaderWriterStateT((_, s) => F.pure((L.empty, s, f(s))))

  /**
   * Like [[inspect]], but using an effectful function.
   */
  def inspectF[F[_], E, L, S, A](f: S => F[A])(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, A] =
    ReaderWriterStateT((_, s) => F.map(f(s))((L.empty, s, _)))

  /**
   * Modify the input state using `f`.
   */
  def modify[F[_], E, L, S](f: S => S)(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, Unit] =
    ReaderWriterStateT((_, s) => F.pure((L.empty, f(s), ())))

  /**
   * Like [[modify]], but using an effectful function.
   */
  def modifyF[F[_], E, L, S](f: S => F[S])(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, Unit] =
    ReaderWriterStateT((_, s) => F.map(f(s))((L.empty, _, ())))

  /**
   * Return the input state without modifying it.
   */
  def get[F[_], E, L, S](implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, S] =
    ReaderWriterStateT((_, s) => F.pure((L.empty, s, s)))

  /**
   * Set the state to `s`.
   */
  def set[F[_], E, L, S](s: S)(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, Unit] =
    ReaderWriterStateT((_, _) => F.pure((L.empty, s, ())))

  /**
   * Like [[set]], but using an effectful `S` value.
   */
  def setF[F[_], E, L, S](fs: F[S])(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, Unit] =
    ReaderWriterStateT((_, _) => F.map(fs)((L.empty, _, ())))

  /**
   * Get the provided environment, without modifying the input state.
   */
  def ask[F[_], E, L, S](implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, E] =
    ReaderWriterStateT((e, s) => F.pure((L.empty, s, e)))

  /**
   * Add a value to the log, without modifying the input state.
   */
  def tell[F[_], E, L, S](l: L)(implicit F: Applicative[F]): ReaderWriterStateT[F, E, L, S, Unit] =
    ReaderWriterStateT((_, s) => F.pure((l, s, ())))

  /**
   * Like [[tell]], but using an effectful `L` value.
   */
  def tellF[F[_], E, L, S](fl: F[L])(implicit F: Applicative[F]): ReaderWriterStateT[F, E, L, S, Unit] =
    ReaderWriterStateT((_, s) => F.map(fl)((_, s, ())))
}

/**
 * Convenience functions for ReaderWriterState.
 */
private[data] abstract class RWSFunctions {
  /**
   * Return `a` and an empty log without modifying the input state.
   */
  def apply[E, L: Monoid, S, A](f: (E, S) => (L, S, A)): ReaderWriterState[E, L, S, A] =
    ReaderWriterStateT.applyF(Now((e, s) => Now(f(e, s))))

  /**
   * Return `a` and an empty log without modifying the input state.
   */
  def pure[E, L: Monoid, S, A](a: A): ReaderWriterState[E, L, S, A] =
    ReaderWriterStateT.pure(a)

  /**
   * Modify the input state using `f`.
   */
  def modify[E, L: Monoid, S](f: S => S): ReaderWriterState[E, L, S, Unit] =
    ReaderWriterStateT.modify(f)

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[E, L: Monoid, S, T](f: S => T): ReaderWriterState[E, L, S, T] =
    ReaderWriterStateT.inspect(f)

  /**
   * Return the input state without modifying it.
   */
  def get[E, L: Monoid, S]: ReaderWriterState[E, L, S, S] =
    ReaderWriterStateT.get

  /**
   * Set the state to `s`.
   */
  def set[E, L: Monoid, S](s: S): ReaderWriterState[E, L, S, Unit] =
    ReaderWriterStateT.set(s)

  /**
   * Get the provided environment, without modifying the input state.
   */
  def ask[E, L, S](implicit L: Monoid[L]): ReaderWriterState[E, L, S, E] =
    ReaderWriterStateT.ask

  /**
   * Add a value to the log, without modifying the input state.
   */
  def tell[E, L, S](l: L): ReaderWriterState[E, L, S, Unit] =
    ReaderWriterStateT.tell(l)
}

private[data] sealed trait RWSTInstances extends RWSTInstances1 {
  implicit def catsDataProfunctorForRWST[F[_], L, S](implicit F0: Functor[F]): Profunctor[ReaderWriterStateT[F, ?, L, S, ?]] =
    new RWSTProfunctor[F, L, S] {
      implicit def F: Functor[F] = F0
    }

  implicit def catsDataContravariantForRWST[F[_], L, S, A](implicit F0: Functor[F]): Contravariant[ReaderWriterStateT[F, ?, L, S, A]] =
    new RWSTContravariant[F, L, S, A] {
      implicit def F: Functor[F] = F0
    }

  implicit def catsDataMonadErrorForRWST[F[_], E, L, S, R](implicit F0: MonadError[F, R], L0: Monoid[L]): MonadError[ReaderWriterStateT[F, E, L, S, ?], R] =
    new RWSTMonadError[F, E, L, S, R] {
      implicit def F: MonadError[F, R] = F0
      implicit def L: Monoid[L] = L0
    }

}

private[data] sealed trait RWSTInstances1 extends RWSTInstances2 {
  implicit def catsDataMonadForRWST[F[_], E, L, S](implicit F0: Monad[F], L0: Monoid[L]): Monad[ReaderWriterStateT[F, E, L, S, ?]] =
    new RWSTMonad[F, E, L, S] {
      implicit def F: Monad[F] = F0
      implicit def L: Monoid[L] = L0
    }
}

private[data] sealed trait RWSTInstances2 extends RWSTInstances3 {
  implicit def catsDataAlternativeForRWST[F[_], E, L, S](
                                                          implicit FM: Monad[F], FA: Alternative[F], L0: Monoid[L]): Alternative[ReaderWriterStateT[F, E, L, S, ?]] =
    new RWSTAlternative[F, E, L, S] {
      implicit def G: Alternative[F] = FA
      implicit def F: Monad[F] = FM
      implicit def L: Monoid[L] = L0
    }
}

private[data] sealed trait RWSTInstances3 {
  implicit def catsDataSemigroupKForRWST[F[_], E, L, S](
                                                         implicit F0: Monad[F], G0: SemigroupK[F]): SemigroupK[ReaderWriterStateT[F, E, L, S, ?]] =
    new RWSTSemigroupK[F, E, L, S] {
      implicit def F: Monad[F] = F0
      implicit def G: SemigroupK[F] = G0
    }
  implicit def catsDataFunctorForRWST[F[_], E, L, S](implicit F0: Functor[F]): Functor[ReaderWriterStateT[F, E, L, S, ?]] =
    new RWSTFunctor[F, E, L, S] {
      implicit def F: Functor[F] = F0
    }
}

private[data] sealed trait RWSTFunctor[F[_], E, L, S] extends Functor[ReaderWriterStateT[F, E, L, S, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: ReaderWriterStateT[F, E, L, S, A])(f: A => B): ReaderWriterStateT[F, E, L, S, B] =
    fa.map(f)
}

private[data] sealed trait RWSTContravariant[F[_], L, S, T] extends Contravariant[ReaderWriterStateT[F, ?, L, S, T]] {
  implicit def F: Functor[F]

  override def contramap[A, B](fa: ReaderWriterStateT[F, A, L, S, T])(f: B => A): ReaderWriterStateT[F, B, L, S, T] =
    fa.contramap(f)
}

private[data] sealed trait RWSTProfunctor[F[_], L, S] extends Profunctor[ReaderWriterStateT[F, ?, L, S, ?]] {
  implicit def F: Functor[F]

  override def dimap[A, B, C, D](fab: ReaderWriterStateT[F, A, L, S, B])(f: C => A)(g: B => D): ReaderWriterStateT[F, C, L, S, D] =
    fab.contramap(f).map(g)
}

private[data] sealed trait RWSTMonad[F[_], E, L, S] extends Monad[ReaderWriterStateT[F, E, L, S, ?]] with RWSTFunctor[F, E, L, S] {
  implicit def F: Monad[F]
  implicit def L: Monoid[L]

  def pure[A](a: A): ReaderWriterStateT[F, E, L, S, A] =
    ReaderWriterStateT.pure(a)

  def flatMap[A, B](fa: ReaderWriterStateT[F, E, L, S, A])(f: A => ReaderWriterStateT[F, E, L, S, B]): ReaderWriterStateT[F, E, L, S, B] =
    fa.flatMap(f)

  def tailRecM[A, B](initA: A)(f: A => ReaderWriterStateT[F, E, L, S, Either[A, B]]): ReaderWriterStateT[F, E, L, S, B] =
    ReaderWriterStateT { (e, initS) =>
      F.tailRecM((L.empty, initS, initA)) { case (currL, currS, currA) =>
        F.map(f(currA).run(e, currS)) { case (nextL, nextS, ab) =>
          ab.bimap((L.combine(currL, nextL), nextS, _), (L.combine(currL, nextL), nextS, _))
        }
      }
    }
}

private[data] sealed trait RWSTSemigroupK[F[_], E, L, S] extends SemigroupK[ReaderWriterStateT[F, E, L, S, ?]] {
  implicit def F: Monad[F]
  implicit def G: SemigroupK[F]

  def combineK[A](x: ReaderWriterStateT[F, E, L, S, A], y: ReaderWriterStateT[F, E, L, S, A]): ReaderWriterStateT[F, E, L, S, A] =
    ReaderWriterStateT { (e, s) =>
      G.combineK(x.run(e, s), y.run(e, s))
    }
}

private[data] sealed trait RWSTAlternative[F[_], E, L, S]
  extends Alternative[ReaderWriterStateT[F, E, L, S, ?]] with RWSTFunctor[F, E, L, S]
    with RWSTSemigroupK[F, E, L, S] {

  implicit def F: Monad[F]
  override def G: Alternative[F]
  implicit def L: Monoid[L]

  def empty[A]: ReaderWriterStateT[F, E, L, S, A] = ReaderWriterStateT.lift(G.empty[A])

  def pure[A](a: A): ReaderWriterStateT[F, E, L, S, A] = ReaderWriterStateT.pure[F, E, L, S, A](a)

  def ap[A, B](ff: ReaderWriterStateT[F, E, L, S, A => B])(fa: ReaderWriterStateT[F, E, L, S, A]): ReaderWriterStateT[F, E, L, S, B] =
    ff.flatMap(f => fa.map(f)(F))(F, L)

}

private[data] sealed trait RWSTMonadError[F[_], E, L, S, R]
    extends RWSTMonad[F, E, L, S] with MonadError[ReaderWriterStateT[F, E, L, S, ?], R] {

  implicit def F: MonadError[F, R]

  def raiseError[A](r: R): ReaderWriterStateT[F, E, L, S, A] = ReaderWriterStateT.lift(F.raiseError(r))

  def handleErrorWith[A](fa: ReaderWriterStateT[F, E, L, S, A])(f: R => ReaderWriterStateT[F, E, L, S, A]): ReaderWriterStateT[F, E, L, S, A] =
    ReaderWriterStateT { (e, s) =>
      F.handleErrorWith(fa.run(e, s))(r => f(r).run(e, s))
    }
}

