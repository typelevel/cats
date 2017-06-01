package cats
package data

import cats.functor.{ Contravariant, Bifunctor, Profunctor }
import cats.syntax.either._

/**
 * Represents a stateful computation in a context `F[_]`, over state `S`, with an initial environment `E`,
 * an accumulated log `L` and a result `A`.
 *
 * In other words, it is a pre-baked stack of `[[ReaderT]][F, E, A]`, `[[WriterT]][F, L, A]`
 * and `[[StateT]][F, S, A]`.
 */
final class ReaderWriterStateT[F[_], E, S, L, A](val runF: F[(E, S) => F[(L, S, A)]]) extends Serializable {

  /**
   * Modify the initial environment using `f`.
   */
  def contramap[E0](f: E0 => E)(implicit F: Functor[F]): ReaderWriterStateT[F, E0, S, L, A] =
    ReaderWriterStateT.applyF {
      F.map(runF) { rwsa =>
        (e0: E0, s: S) => rwsa(f(e0), s)
      }
    }

  /**
   * Alias for [[contramap]].
   */
  def local[EE](f: EE => E)(implicit F: Functor[F]): ReaderWriterStateT[F, EE, S, L, A] =
    contramap(f)

  /**
   * Modify the result of the computation using `f`.
   */
  def map[B](f: A => B)(implicit F: Functor[F]): ReaderWriterStateT[F, E, S, L, B] =
    transform { (l, s, a) => (l, s, f(a)) }

  /**
   * Modify the written log value using `f`.
   */
  def mapWritten[LL](f: L => LL)(implicit F: Functor[F]): ReaderWriterStateT[F, E, S, LL, A] =
    transform { (l, s, a) => (f(l), s, a) }

  /**
   * Modify the result of the computation by feeding it into `f`, threading the state
   * through the resulting computation and combining the log values.
   */
  def flatMap[B](f: A => ReaderWriterStateT[F, E, S, L, B])(
    implicit F: FlatMap[F], L: Semigroup[L]): ReaderWriterStateT[F, E, S, L, B] =
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
  def flatMapF[B](faf: A => F[B])(implicit F: FlatMap[F]): ReaderWriterStateT[F, E, S, L, B] =
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
  def transform[LL, B](f: (L, S, A) => (LL, S, B))(implicit F: Functor[F]): ReaderWriterStateT[F, E, S, LL, B] =
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
    implicit F: Monad[F], G: Applicative[G]): ReaderWriterStateT[G, E, S, LL, B] =
    ReaderWriterStateT.apply((e, s) => f(run(e, s)))


  /**
   * Modify the resulting state.
   */
  def modify(f: S => S)(implicit F: Functor[F]): ReaderWriterStateT[F, E, S, L, A] =
    transform { (l, s, a) => (l, f(s), a) }

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[B](f: S => B)(implicit F: Functor[F]): ReaderWriterStateT[F, E, S, L, B] =
    transform { (l, s, a) => (l, s, f(s)) }

  /**
   * Get the input state, without modifying it.
   */
  def get(implicit F: Functor[F]): ReaderWriterStateT[F, E, S, L, S] =
    inspect(identity)

  /**
   * Add a value to the log.
   */
  def tell(l: L)(implicit F: Functor[F], L: Semigroup[L]): ReaderWriterStateT[F, E, S, L, A] =
    mapWritten(L.combine(_, l))

  /**
   * Retrieve the value written to the log.
   */
  def written(implicit F: Functor[F]): ReaderWriterStateT[F, E, S, L, L] =
    transform { (l, s, a) => (l, s, l) }

  /**
   * Clear the log.
   */
  def reset(implicit F: Functor[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, A] =
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
  def apply[F[_], E, S, L, A](runF: (E, S) => F[(L, S, A)])(implicit F: Applicative[F]): ReaderWriterStateT[F, E, S, L, A] =
    new ReaderWriterStateT(F.pure(runF))

  /**
   * Like [[apply]], but using a function in a context `F`.
   */
  def applyF[F[_], E, S, L, A](runF: F[(E, S) => F[(L, S, A)]]): ReaderWriterStateT[F, E, S, L, A] =
    new ReaderWriterStateT(runF)

  /**
   * Return `a` and an empty log without modifying the input state.
   */
  def pure[F[_], E, S, L, A](a: A)(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, A] =
    ReaderWriterStateT((_, s) => F.pure((L.empty, s, a)))

  /**
   * Return an effectful `a` and an empty log without modifying the input state.
   */
  def lift[F[_], E, S, L, A](fa: F[A])(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, A] =
    ReaderWriterStateT((_, s) => F.map(fa)((L.empty, s, _)))

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[F[_], E, S, L, A](f: S => A)(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, A] =
    ReaderWriterStateT((_, s) => F.pure((L.empty, s, f(s))))

  /**
   * Like [[inspect]], but using an effectful function.
   */
  def inspectF[F[_], E, S, L, A](f: S => F[A])(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, A] =
    ReaderWriterStateT((_, s) => F.map(f(s))((L.empty, s, _)))

  /**
   * Modify the input state using `f`.
   */
  def modify[F[_], E, S, L](f: S => S)(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, Unit] =
    ReaderWriterStateT((_, s) => F.pure((L.empty, f(s), ())))

  /**
   * Like [[modify]], but using an effectful function.
   */
  def modifyF[F[_], E, S, L](f: S => F[S])(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, Unit] =
    ReaderWriterStateT((_, s) => F.map(f(s))((L.empty, _, ())))

  /**
   * Return the input state without modifying it.
   */
  def get[F[_], E, S, L](implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, S] =
    ReaderWriterStateT((_, s) => F.pure((L.empty, s, s)))

  /**
   * Set the state to `s`.
   */
  def set[F[_], E, S, L](s: S)(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, Unit] =
    ReaderWriterStateT((_, _) => F.pure((L.empty, s, ())))

  /**
   * Like [[set]], but using an effectful `S` value.
   */
  def setF[F[_], E, S, L](fs: F[S])(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, Unit] =
    ReaderWriterStateT((_, _) => F.map(fs)((L.empty, _, ())))

  /**
   * Get the provided environment, without modifying the input state.
   */
  def ask[F[_], E, S, L](implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, S, L, E] =
    ReaderWriterStateT((e, s) => F.pure((L.empty, s, e)))

  /**
   * Add a value to the log, without modifying the input state.
   */
  def tell[F[_], E, S, L](l: L)(implicit F: Applicative[F]): ReaderWriterStateT[F, E, S, L, Unit] =
    ReaderWriterStateT((_, s) => F.pure((l, s, ())))

  /**
   * Like [[tell]], but using an effectful `L` value.
   */
  def tellF[F[_], E, S, L](fl: F[L])(implicit F: Applicative[F]): ReaderWriterStateT[F, E, S, L, Unit] =
    ReaderWriterStateT((_, s) => F.map(fl)((_, s, ())))
}

/**
 * Convenience functions for ReaderWriterState.
 */
private[data] abstract class RWSFunctions {
  /**
   * Return `a` and an empty log without modifying the input state.
   */
  def apply[E, S, L: Monoid, A](f: (E, S) => (L, S, A)): ReaderWriterState[E, S, L, A] =
    ReaderWriterStateT.applyF(Now((e, s) => Now(f(e, s))))

  /**
   * Return `a` and an empty log without modifying the input state.
   */
  def pure[E, S, L: Monoid, A](a: A): ReaderWriterState[E, S, L, A] =
    ReaderWriterStateT.pure(a)

  /**
   * Modify the input state using `f`.
   */
  def modify[E, S, L: Monoid](f: S => S): ReaderWriterState[E, S, L, Unit] =
    ReaderWriterStateT.modify(f)

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[E, S, L: Monoid, T](f: S => T): ReaderWriterState[E, S, L, T] =
    ReaderWriterStateT.inspect(f)

  /**
   * Return the input state without modifying it.
   */
  def get[E, S, L: Monoid]: ReaderWriterState[E, S, L, S] =
    ReaderWriterStateT.get

  /**
   * Set the state to `s`.
   */
  def set[E, S, L: Monoid](s: S): ReaderWriterState[E, S, L, Unit] =
    ReaderWriterStateT.set(s)

  /**
   * Get the provided environment, without modifying the input state.
   */
  def ask[E, S, L](implicit L: Monoid[L]): ReaderWriterState[E, S, L, E] =
    ReaderWriterStateT.ask

  /**
   * Add a value to the log, without modifying the input state.
   */
  def tell[E, S, L](l: L): ReaderWriterState[E, S, L, Unit] =
    ReaderWriterStateT.tell(l)
}

private[data] sealed trait RWSTInstances extends RWSTInstances1 {
  implicit def catsDataMonadStateForRWST[F[_], E, S, L](
    implicit F0: Monad[F], L0: Monoid[L]): MonadState[ReaderWriterStateT[F, E, S, L, ?], S] =
    new RWSTMonadState[F, E, S, L] {
      implicit def F: Monad[F] = F0
      implicit def L: Monoid[L] = L0
    }

  implicit def catsDataMonadTransForRWST[E, S, L](
    implicit L0: Monoid[L]): MonadTrans[ReaderWriterStateT[?[_], E, S, L, ?]] =
    new RWSTMonadTrans[E, S, L] {
      implicit def L: Monoid[L] = L0
    }

  implicit def catsDataProfunctorForRWST[F[_], S, L](implicit F0: Functor[F]): Profunctor[ReaderWriterStateT[F, ?, S, L, ?]] =
    new RWSTProfunctor[F, S, L] {
      implicit def F: Functor[F] = F0
    }

  implicit def catsDataBifunctorForRWST[F[_], E, S](implicit F0: Functor[F]): Bifunctor[ReaderWriterStateT[F, E, S, ?, ?]] =
    new RWSTBifunctor[F, E, S] {
      implicit def F: Functor[F] = F0
    }

  implicit def catsDataContravariantForRWST[F[_], S, L, A](implicit F0: Functor[F]): Contravariant[ReaderWriterStateT[F, ?, S, L, A]] =
    new RWSTContravariant[F, S, L, A] {
      implicit def F: Functor[F] = F0
    }
}

private[data] sealed trait RWSTInstances1 extends RWSTInstances2 {
  implicit def catsDataMonadCombineForRWST[F[_], E, S, L](
    implicit F0: MonadCombine[F], L0: Monoid[L]): MonadCombine[ReaderWriterStateT[F, E, S, L, ?]] =
    new RWSTMonadCombine[F, E, S, L] {
      implicit def F: MonadCombine[F] = F0
      implicit def L: Monoid[L] = L0
    }
}

private[data] sealed trait RWSTInstances2 extends RWSTInstances3 {
  implicit def catsDataMonadErrorForRWST[F[_], E, S, L, R](
    implicit F0: MonadError[F, R], L0: Monoid[L]): MonadError[ReaderWriterStateT[F, E, S, L, ?], R] =
    new RWSTMonadError[F, E, S, L, R] {
      implicit def F: MonadError[F, R] = F0
      implicit def L: Monoid[L] = L0
    }

  implicit def catsDataSemigroupKForRWST[F[_], E, S, L](
    implicit F0: Monad[F], G0: SemigroupK[F]): SemigroupK[ReaderWriterStateT[F, E, S, L, ?]] =
    new RWSTSemigroupK[F, E, S, L] {
      implicit def F: Monad[F] = F0
      implicit def G: SemigroupK[F] = G0
    }
}

private[data] sealed trait RWSTInstances3 extends RWSTInstances4 {
  implicit def catsDataMonadReaderForRWST[F[_], E, S, L](
    implicit F0: Monad[F], L0: Monoid[L]): MonadReader[ReaderWriterStateT[F, E, S, L, ?], E] =
    new RWSTMonadReader[F, E, S, L] {
      implicit def F: Monad[F] = F0
      implicit def L: Monoid[L] = L0
    }
}

private[data] sealed trait RWSTInstances4 extends RWSTInstances5 {
  implicit def catsDataMonadWriterForRWST[F[_], E, S, L](
    implicit F0: Monad[F], L0: Monoid[L]): MonadWriter[ReaderWriterStateT[F, E, S, L, ?], L] =
    new RWSTMonadWriter[F, E, S, L] {
      implicit def F: Monad[F] = F0
      implicit def L: Monoid[L] = L0
    }
}

private[data] sealed trait RWSTInstances5 extends RWSTInstances6 {
  implicit def catsDataMonadForRWST[F[_], E, S, L](implicit F0: Monad[F], L0: Monoid[L]): Monad[ReaderWriterStateT[F, E, S, L, ?]] =
    new RWSTMonad[F, E, S, L] {
      implicit def F: Monad[F] = F0
      implicit def L: Monoid[L] = L0
    }
}

private[data] sealed trait RWSTInstances6 {
  implicit def catsDataFunctorForRWST[F[_], E, S, L](implicit F0: Functor[F]): Functor[ReaderWriterStateT[F, E, S, L, ?]] =
    new RWSTFunctor[F, E, S, L] {
      implicit def F: Functor[F] = F0
    }
}

private[data] sealed trait RWSTFunctor[F[_], E, S, L] extends Functor[ReaderWriterStateT[F, E, S, L, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: ReaderWriterStateT[F, E, S, L, A])(f: A => B): ReaderWriterStateT[F, E, S, L, B] =
    fa.map(f)
}

private[data] sealed trait RWSTContravariant[F[_], S, L, T] extends Contravariant[ReaderWriterStateT[F, ?, S, L, T]] {
  implicit def F: Functor[F]

  override def contramap[A, B](fa: ReaderWriterStateT[F, A, S, L, T])(f: B => A): ReaderWriterStateT[F, B, S, L, T] =
    fa.contramap(f)
}

private[data] sealed trait RWSTBifunctor[F[_], E, S] extends Bifunctor[ReaderWriterStateT[F, E, S, ?, ?]] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: ReaderWriterStateT[F, E, S, A, B])(
    f: A => C, g: B => D): ReaderWriterStateT[F, E, S, C, D] = fab.mapWritten(f).map(g)
}

private[data] sealed trait RWSTProfunctor[F[_], S, L] extends Profunctor[ReaderWriterStateT[F, ?, S, L, ?]] {
  implicit def F: Functor[F]

  override def dimap[A, B, C, D](fab: ReaderWriterStateT[F, A, S, L, B])(f: C => A)(g: B => D): ReaderWriterStateT[F, C, S, L, D] =
    fab.contramap(f).map(g)
}

private[data] sealed trait RWSTMonad[F[_], E, S, L] extends Monad[ReaderWriterStateT[F, E, S, L, ?]] with RWSTFunctor[F, E, S, L] {
  implicit def F: Monad[F]
  implicit def L: Monoid[L]

  def pure[A](a: A): ReaderWriterStateT[F, E, S, L, A] =
    ReaderWriterStateT.pure(a)

  def flatMap[A, B](fa: ReaderWriterStateT[F, E, S, L, A])(f: A => ReaderWriterStateT[F, E, S, L, B]): ReaderWriterStateT[F, E, S, L, B] =
    fa.flatMap(f)

  def tailRecM[A, B](initA: A)(f: A => ReaderWriterStateT[F, E, S, L, Either[A, B]]): ReaderWriterStateT[F, E, S, L, B] =
    ReaderWriterStateT { (e, initS) =>
      F.tailRecM((L.empty, initS, initA)) { case (currL, currS, currA) =>
        F.map(f(currA).run(e, currS)) { case (nextL, nextS, ab) =>
          ab.bimap((L.combine(currL, nextL), nextS, _), (L.combine(currL, nextL), nextS, _))
        }
      }
    }
}

private[data] sealed trait RWSTMonadState[F[_], E, S, L]
    extends MonadState[ReaderWriterStateT[F, E, S, L, ?], S] with RWSTMonad[F, E, S, L] {

  lazy val get: ReaderWriterStateT[F, E, S, L, S] = ReaderWriterStateT.get

  def set(s: S): ReaderWriterStateT[F, E, S, L, Unit] = ReaderWriterStateT.set(s)
}

private[data] sealed trait RWSTMonadTrans[E, S, L] extends MonadTrans[ReaderWriterStateT[?[_], E, S, L, ?]] {
  implicit def L: Monoid[L]
  def liftT[M[_]: Monad, A](ma: M[A]): ReaderWriterStateT[M, E, S, L, A] =
    ReaderWriterStateT.lift(ma)
}

private[data] sealed trait RWSTSemigroupK[F[_], E, S, L] extends SemigroupK[ReaderWriterStateT[F, E, S, L, ?]] {
  implicit def F: Monad[F]
  implicit def G: SemigroupK[F]

  def combineK[A](x: ReaderWriterStateT[F, E, S, L, A], y: ReaderWriterStateT[F, E, S, L, A]): ReaderWriterStateT[F, E, S, L, A] =
    ReaderWriterStateT { (e, s) =>
      G.combineK(x.run(e, s), y.run(e, s))
    }
}

private[data] sealed trait RWSTMonadCombine[F[_], E, S, L]
    extends MonadCombine[ReaderWriterStateT[F, E, S, L, ?]] with RWSTMonad[F, E, S, L]
    with RWSTSemigroupK[F, E, S, L] with RWSTMonadTrans[E, S, L] {

  implicit def F: MonadCombine[F]
  override def G: MonadCombine[F] = F

  def empty[A]: ReaderWriterStateT[F, E, S, L, A] = liftT[F, A](F.empty[A])
}

private[data] sealed trait RWSTMonadError[F[_], E, S, L, R]
    extends RWSTMonad[F, E, S, L] with MonadError[ReaderWriterStateT[F, E, S, L, ?], R] {

  implicit def F: MonadError[F, R]

  def raiseError[A](r: R): ReaderWriterStateT[F, E, S, L, A] = ReaderWriterStateT.lift(F.raiseError(r))

  def handleErrorWith[A](fa: ReaderWriterStateT[F, E, S, L, A])(f: R => ReaderWriterStateT[F, E, S, L, A]): ReaderWriterStateT[F, E, S, L, A] =
    ReaderWriterStateT { (e, s) =>
      F.handleErrorWith(fa.run(e, s))(r => f(r).run(e, s))
    }
}

private[data] sealed trait RWSTMonadReader[F[_], E, S, L]
    extends RWSTMonad[F, E, S, L] with MonadReader[ReaderWriterStateT[F, E, S, L, ?], E] {

  val ask: ReaderWriterStateT[F, E, S, L, E] = ReaderWriterStateT.ask

  def local[A](f: E => E)(fa: ReaderWriterStateT[F, E, S, L, A]): ReaderWriterStateT[F, E, S, L, A] = fa contramap f
}

private[data] sealed trait RWSTMonadWriter[F[_], E, S, L]
    extends RWSTMonad[F, E, S, L] with MonadWriter[ReaderWriterStateT[F, E, S, L, ?], L] {

  def writer[A](aw: (L, A)): ReaderWriterStateT[F, E, S, L, A] =
    ReaderWriterStateT((_, s) => F.pure((aw._1, s, aw._2)))

  def listen[A](fa: ReaderWriterStateT[F, E, S, L, A]): ReaderWriterStateT[F, E, S, L, (L, A)] =
    fa.transform { (l, s, a) =>
      (l, s, (l, a))
    }

  def pass[A](fa: ReaderWriterStateT[F, E, S, L, (L => L, A)]): ReaderWriterStateT[F, E, S, L, A] =
    fa.transform { case (l, s, (fl, a)) =>
      (fl(l), s, a)
    }
}
