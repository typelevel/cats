/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats
package data

import cats.arrow.{Profunctor, Strong}

/**
 * Represents a stateful computation in a context `F[_]`, from state `SA` to state `SB`,
 *  with an initial environment `E`, an accumulated log `L` and a result `A`.
 *
 * In other words, it is a pre-baked stack of `[[ReaderT]][F, E, A]`, `[[WriterT]][F, L, A]`
 * and `[[IndexedStateT]][F, SA, SB, A]`.
 */
final class IndexedReaderWriterStateT[F[_], E, L, SA, SB, A](val runF: F[(E, SA) => F[(L, SB, A)]])
    extends Serializable {

  /**
   * Modify the initial state using `f`.
   */
  def contramap[S0](f: S0 => SA)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, L, S0, SB, A] =
    IndexedReaderWriterStateT.applyF {
      F.map(runF) { rwsfa => (e: E, s0: S0) =>
        rwsfa(e, f(s0))
      }
    }

  /**
   * Modify the initial environment using `f`.
   *
   * {{{
   * scala> import cats.syntax.all._
   * scala> type Env = String
   * scala> type GlobalEnv = (Int, Env)
   * scala> type Log = List[String]
   * scala> val xLocal: IndexedReaderWriterStateT[Option, Env, Log, Int, Int, Int] = IndexedReaderWriterStateT.get
   * scala> val xGlobal: IndexedReaderWriterStateT[Option, GlobalEnv, Log, Int, Int, Int] = xLocal.local(_._2)
   * scala> val globalEnv: GlobalEnv = (5, "env")
   * scala> xGlobal.run(globalEnv, 5)
   * res0: Option[(List[String], Int, Int)] = Some((List(),5,5))
   * }}}
   */
  def local[EE](f: EE => E)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, EE, L, SA, SB, A] =
    IndexedReaderWriterStateT.applyF {
      F.map(runF) { rwsa => (ee: EE, sa: SA) =>
        rwsa(f(ee), sa)
      }
    }

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val x: IndexedReaderWriterStateT[Option, String, String, Int, Int, Unit] = IndexedReaderWriterStateT.tell("something")
   * scala> val y: IndexedReaderWriterStateT[Option, String, String, Int, Int, (Unit, String)] = x.listen
   * scala> y.run("environment", 17)
   * res0: Option[(String, Int, (Unit, String))] = Some((something,17,((),something)))
   * }}}
   */
  def listen(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, L, SA, SB, (A, L)] =
    transform { (l, s, a) =>
      (l, s, (a, l))
    }

  /**
   * Modify the result of the computation using `f`.
   */
  def map[B](f: A => B)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, L, SA, SB, B] =
    transform { (l, s, a) =>
      (l, s, f(a))
    }

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): IndexedReaderWriterStateT[G, E, L, SA, SB, A] =
    IndexedReaderWriterStateT.applyF(f(F.map(runF)(rwsa => (e, sa) => f(rwsa(e, sa)))))

  /**
   * Modify the resulting state using `f` and the resulting value using `g`.
   */
  def bimap[SC, B](f: SB => SC, g: A => B)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, L, SA, SC, B] =
    transform { (l, s, a) =>
      (l, f(s), g(a))
    }

  /**
   * Modify the initial state using `f` and the resulting state using `g`.
   */
  def dimap[S0, S1](f: S0 => SA)(g: SB => S1)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, L, S0, S1, A] =
    contramap(f).modify(g)

  /**
   * Modify the written log value using `f`.
   */
  def mapWritten[LL](f: L => LL)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, LL, SA, SB, A] =
    transform { (l, s, a) =>
      (f(l), s, a)
    }

  /**
   * Modify the result of the computation by feeding it into `f`, threading the state
   * through the resulting computation and combining the log values.
   */
  def flatMap[SC, B](
    f: A => IndexedReaderWriterStateT[F, E, L, SB, SC, B]
  )(implicit F: FlatMap[F], L: Semigroup[L]): IndexedReaderWriterStateT[F, E, L, SA, SC, B] =
    IndexedReaderWriterStateT.shift {
      F.map(runF) { rwsfa => (e: E, sa: SA) =>
        F.flatMap(rwsfa(e, sa)) { case (la, sb, a) =>
          F.flatMap(f(a).runF) { rwsfb =>
            F.map(rwsfb(e, sb)) { case (lb, sc, b) =>
              (L.combine(la, lb), sc, b)
            }
          }
        }
      }
    }

  /**
   * Like [[map]], but allows the mapping function to return an effectful value.
   */
  def flatMapF[B](faf: A => F[B])(implicit F: FlatMap[F]): IndexedReaderWriterStateT[F, E, L, SA, SB, B] =
    IndexedReaderWriterStateT.shift {
      F.map(runF) { rwsfa => (e: E, sa: SA) =>
        F.flatMap(rwsfa(e, sa)) { case (l, sb, a) =>
          F.map(faf(a))((l, sb, _))
        }
      }
    }

  /**
   * Transform the resulting log, state and value using `f`.
   */
  def transform[LL, SC, B](
    f: (L, SB, A) => (LL, SC, B)
  )(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, LL, SA, SC, B] =
    IndexedReaderWriterStateT.applyF {
      F.map(runF) { rwsfa => (e: E, s: SA) =>
        F.map(rwsfa(e, s)) { case (l, sb, a) =>
          val (ll, sc, b) = f(l, sb, a)
          (ll, sc, b)
        }
      }
    }

  /**
   * Like [[transform]], but allows the context to change from `F` to `G`.
   *
   * {{{
   * scala> import cats.syntax.all._
   * scala> type ErrorOr[A] = Either[String, A]
   * scala> type Env = String
   * scala> type Log = List[String]
   * scala> val xError: IndexedReaderWriterStateT[ErrorOr, Env, Log, Int, Int, Int] = IndexedReaderWriterStateT.get
   * scala> val xOpt: IndexedReaderWriterStateT[Option, Env, Log, Int, Int, Int] = xError.transformF(_.toOption)
   * scala> val input = 5
   * scala> xError.run("env", input)
   * res0: ErrorOr[(Log, Int, Int)] = Right((List(),5,5))
   * scala> xOpt.run("env", 5)
   * res1: Option[(Log, Int, Int)] = Some((List(),5,5))
   * }}}
   */
  def transformF[G[_], LL, SC, B](
    f: F[(L, SB, A)] => G[(LL, SC, B)]
  )(implicit F: Monad[F], G: Applicative[G]): IndexedReaderWriterStateT[G, E, LL, SA, SC, B] =
    IndexedReaderWriterStateT.apply((e, s) => f(run(e, s)))

  /**
   * Like [[transform]], but does it in the monadic context `F`.
   *
   * {{{
   * scala> import cats.syntax.all._
   * scala> type Env = String
   * scala> type Log = List[String]
   * scala> val xOpt0: IndexedReaderWriterStateT[Option, Env, Log, Int, Int, Int] = IndexedReaderWriterStateT.get
   * scala> val xOpt: IndexedReaderWriterStateT[Option, Env, Log, Int, Int, Int] = xOpt0.tell("xxx" :: Nil)
   * scala> val xHead: IndexedReaderWriterStateT[Option, Env, Log, Int, Int, String] = xOpt.semiflatTransform((l, s, a) => l.headOption.map(h => (l, s, h + a)))
   * scala> val input = 5
   * scala> xOpt.run("env", input)
   * res0: Option[(Log, Int, Int)] = Some((List(xxx),5,5))
   * scala> xHead.run("env", 5)
   * res1: Option[(Log, Int, String)] = Some((List(xxx),5,xxx5))
   * }}}
   */
  def semiflatTransform[LL, SC, B](
    f: (L, SB, A) => F[(LL, SC, B)]
  )(implicit F: Monad[F]): IndexedReaderWriterStateT[F, E, LL, SA, SC, B] =
    IndexedReaderWriterStateT.apply((e, s) => F.flatMap(run(e, s)) { case (l, sb, a) => f(l, sb, a) })

  /**
   * Transform the state used. See [[StateT]] for more details.
   *
   * {{{
   * scala> import cats.syntax.all._ // needed for StateT.apply
   * scala> type Env = String
   * scala> type Log = List[String]
   * scala> type S[SA, SB, A] = IndexedReaderWriterStateT[Option, Env, Log, SA, SB, A]
   * scala> type GlobalEnv = (Int, String)
   * scala> val x: S[Int, Int, Double] = IndexedReaderWriterStateT((env: Env, x: Int) => Option(("Addition" :: Nil, x + 1, x.toDouble)))
   * scala> val xt: S[GlobalEnv, GlobalEnv, Double] = x.transformS[GlobalEnv](_._1, (t, i) => (i, t._2))
   * scala> val input = 5
   * scala> x.run("env", input)
   * res0: Option[(Log, Int, Double)] = Some((List(Addition),6,5.0))
   * scala> xt.run("env", (input, "hello"))
   * res1: Option[(Log, GlobalEnv, Double)] = Some((List(Addition),(6,hello),5.0))
   * }}}
   */
  def transformS[R](f: R => SA, g: (R, SB) => R)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, L, R, R, A] =
    IndexedReaderWriterStateT.applyF {
      F.map(runF) { rwsfa => (e: E, r: R) =>
        F.map(rwsfa(e, f(r))) { case (l, sb, a) =>
          (l, g(r, sb), a)
        }
      }
    }

  /**
   * Modify the resulting state.
   */
  def modify[SC](f: SB => SC)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, L, SA, SC, A] =
    transform { (l, sb, a) =>
      (l, f(sb), a)
    }

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[B](f: SB => B)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, L, SA, SB, B] =
    transform { (l, sb, _) =>
      (l, sb, f(sb))
    }

  /**
   * Inspect a value from the environment and input state, without modifying the state.
   *
   * {{{
   * scala> import cats.syntax.all._
   * scala> type Env = String
   * scala> type Log = List[String]
   * scala> val xOpt: IndexedReaderWriterStateT[Option, Env, Log, Int, Int, Int] = IndexedReaderWriterStateT.get
   * scala> val xAsk: IndexedReaderWriterStateT[Option, Env, Log, Int, Int, String] = xOpt.inspectAsk(_ + _)
   * scala> val input = 5
   * scala> xOpt.run("env", input)
   * res0: Option[(Log, Int, Int)] = Some((List(),5,5))
   * scala> xAsk.run("env", 5)
   * res1: Option[(Log, Int, String)] = Some((List(),5,env5))
   * }}}
   */
  def inspectAsk[B](f: (E, SB) => B)(implicit F: Monad[F]): IndexedReaderWriterStateT[F, E, L, SA, SB, B] =
    IndexedReaderWriterStateT.apply { (e, sa) =>
      F.map(run(e, sa)) { case (l, sb, _) => (l, sb, f(e, sb)) }
    }

  /**
   * Get the input state, without modifying it.
   */
  def get(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, L, SA, SB, SB] =
    inspect(identity)

  /**
   * Add a value to the log.
   */
  def tell(l: L)(implicit F: Functor[F], L: Semigroup[L]): IndexedReaderWriterStateT[F, E, L, SA, SB, A] =
    mapWritten(L.combine(_, l))

  /**
   * Retrieve the value written to the log.
   */
  def written(implicit F: Functor[F]): IndexedReaderWriterStateT[F, E, L, SA, SB, L] =
    transform { (l, sb, _) =>
      (l, sb, l)
    }

  /**
   * Clear the log.
   */
  def reset(implicit F: Functor[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, SA, SB, A] =
    mapWritten(_ => L.empty)

  /**
   * Run the computation using the provided initial environment and state.
   */
  def run(env: E, initial: SA)(implicit F: Monad[F]): F[(L, SB, A)] =
    F.flatMap(runF)(_.apply(env, initial))

  /**
   * Run the computation using the provided environment and an empty state.
   */
  def runEmpty(env: E)(implicit F: Monad[F], SA: Monoid[SA]): F[(L, SB, A)] =
    run(env, SA.empty)

  /**
   * Like [[run]], but discards the final state and log.
   */
  def runA(env: E, initial: SA)(implicit F: Monad[F]): F[A] =
    F.map(run(env, initial))(_._3)

  /**
   * Like [[run]], but discards the final value and log.
   */
  def runS(env: E, initial: SA)(implicit F: Monad[F]): F[SB] =
    F.map(run(env, initial))(_._2)

  /**
   * Like [[run]], but discards the final state and value.
   */
  def runL(env: E, initial: SA)(implicit F: Monad[F]): F[L] =
    F.map(run(env, initial))(_._1)

  /**
   * Like [[runEmpty]], but discards the final state and log.
   */
  def runEmptyA(env: E)(implicit F: Monad[F], SA: Monoid[SA]): F[A] =
    runA(env, SA.empty)

  /**
   * Like [[runEmpty]], but discards the final value and log.
   */
  def runEmptyS(env: E)(implicit F: Monad[F], SA: Monoid[SA]): F[SB] =
    runS(env, SA.empty)

  /**
   * Like [[runEmpty]], but discards the final state and value.
   */
  def runEmptyL(env: E)(implicit F: Monad[F], SA: Monoid[SA]): F[L] =
    runL(env, SA.empty)
}

sealed private[data] trait CommonIRWSTConstructors {

  /**
   * Return `a` and an empty log without modifying the input state.
   */
  def pure[F[_], E, L, S, A](
    a: A
  )(implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, S, S, A] =
    IndexedReaderWriterStateT((_, s) => F.pure((L.empty, s, a)))

  /**
   * Return an effectful `a` and an empty log without modifying the input state.
   */
  def liftF[F[_], E, L, S, A](
    fa: F[A]
  )(implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, S, S, A] =
    IndexedReaderWriterStateT((_, s) => F.map(fa)((L.empty, s, _)))

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, *]]
   * scala> val b: OptionT[RWST[Eval, Boolean, List[String], String, *], Int] = a.mapK(RWST.liftK)
   * scala> b.value.runEmpty(true).value
   * res0: (List[String], String, Option[Int]) = (List(),"",Some(1))
   * }}}
   */
  def liftK[F[_], E, L, S](implicit F: Applicative[F], L: Monoid[L]): F ~> IndexedReaderWriterStateT[F, E, L, S, S, *] =
    new (F ~> IndexedReaderWriterStateT[F, E, L, S, S, *]) {
      def apply[A](a: F[A]): IndexedReaderWriterStateT[F, E, L, S, S, A] = IndexedReaderWriterStateT.liftF(a)
    }

  @deprecated("Use liftF instead", "1.0.0-RC2")
  def lift[F[_], E, L, S, A](
    fa: F[A]
  )(implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, S, S, A] =
    IndexedReaderWriterStateT((_, s) => F.map(fa)((L.empty, s, _)))

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[F[_], E, L, S, A](
    f: S => A
  )(implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, S, S, A] =
    IndexedReaderWriterStateT((_, s) => F.pure((L.empty, s, f(s))))

  /**
   * Like [[inspect]], but using an effectful function.
   */
  def inspectF[F[_], E, L, S, A](
    f: S => F[A]
  )(implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, S, S, A] =
    IndexedReaderWriterStateT((_, s) => F.map(f(s))((L.empty, s, _)))

  /**
   * Inspect values from the environment and input state, without modifying the state.
   */
  def inspectAsk[F[_]: Applicative, E, L: Monoid, S, A](f: (E, S) => A): ReaderWriterStateT[F, E, L, S, A] =
    IndexedReaderWriterStateT((e, s) => Applicative[F].pure((Monoid[L].empty, s, f(e, s))))

  /**
   * Like [[inspectAsk]], but using an effectful function.
   */
  def inspectAskF[F[_]: Applicative, E, L: Monoid, S, A](f: (E, S) => F[A]): ReaderWriterStateT[F, E, L, S, A] =
    IndexedReaderWriterStateT((e, s) => Functor[F].map(f(e, s))((Monoid[L].empty, s, _)))

  /**
   * Set the state to `s`.
   */
  def set[F[_], E, L, S](
    s: S
  )(implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, S, S, Unit] =
    IndexedReaderWriterStateT((_, _) => F.pure((L.empty, s, ())))

  /**
   * Like [[set]], but using an effectful `S` value.
   */
  def setF[F[_], E, L, S](
    fs: F[S]
  )(implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, S, S, Unit] =
    IndexedReaderWriterStateT((_, _) => F.map(fs)((L.empty, _, ())))

  /**
   * Get the provided environment, without modifying the input state.
   */
  def ask[F[_], E, L, S](implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, S, S, E] =
    IndexedReaderWriterStateT((e, s) => F.pure((L.empty, s, e)))

  /**
   * Add a value to the log, without modifying the input state.
   */
  def tell[F[_], E, L, S](l: L)(implicit F: Applicative[F]): IndexedReaderWriterStateT[F, E, L, S, S, Unit] =
    IndexedReaderWriterStateT((_, s) => F.pure((l, s, ())))

  /**
   * Like [[tell]], but using an effectful `L` value.
   */
  def tellF[F[_], E, L, S](fl: F[L])(implicit F: Applicative[F]): IndexedReaderWriterStateT[F, E, L, S, S, Unit] =
    IndexedReaderWriterStateT((_, s) => F.map(fl)((_, s, ())))

  /**
   * Return the input state without modifying it.
   */
  def get[F[_], E, L, S](implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, S, S, S] =
    IndexedReaderWriterStateT((_, s) => F.pure((L.empty, s, s)))
}

object IndexedReaderWriterStateT extends IRWSTInstances with CommonIRWSTConstructors {

  /**
   * Construct a new computation using the provided function.
   */
  def apply[F[_], E, L, SA, SB, A](
    runF: (E, SA) => F[(L, SB, A)]
  )(implicit F: Applicative[F]): IndexedReaderWriterStateT[F, E, L, SA, SB, A] =
    new IndexedReaderWriterStateT(F.pure(runF))

  /**
   * Like [[apply]], but using a function in a context `F`.
   */
  def applyF[F[_], E, L, SA, SB, A](runF: F[(E, SA) => F[(L, SB, A)]]): IndexedReaderWriterStateT[F, E, L, SA, SB, A] =
    new IndexedReaderWriterStateT(runF)

  /**
   * Modify the input state using `f`.
   */
  def modify[F[_], E, L, SA, SB](
    f: SA => SB
  )(implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, SA, SB, Unit] =
    IndexedReaderWriterStateT((_, s) => F.pure((L.empty, f(s), ())))

  /**
   * Like [[modify]], but using an effectful function.
   */
  def modifyF[F[_], E, L, SA, SB](
    f: SA => F[SB]
  )(implicit F: Applicative[F], L: Monoid[L]): IndexedReaderWriterStateT[F, E, L, SA, SB, Unit] =
    IndexedReaderWriterStateT((_, s) => F.map(f(s))((L.empty, _, ())))

  /**
   * Internal API — shifts the execution of `run` in the `F` context.
   *
   * Used to build IndexedReaderWriterStateT values for `F[_]` data types that implement `Monad`,
   * in which case it is safer to trigger the `F[_]` context earlier.
   *
   * This is needed for [[IndexedReaderWriterStateT.flatMap]] to be stack-safe when the underlying F[_] is,
   * for further explanation see [[Kleisli.shift]].
   */
  private[data] def shift[F[_], E, L, SA, SB, A](
    runF: F[(E, SA) => F[(L, SB, A)]]
  )(implicit F: FlatMap[F]): IndexedReaderWriterStateT[F, E, L, SA, SB, A] =
    F match {
      case ap: Applicative[F] @unchecked =>
        IndexedReaderWriterStateT.apply[F, E, L, SA, SB, A]((e: E, sa: SA) => F.flatMap(runF)(f => f(e, sa)))(ap)
      case _ =>
        IndexedReaderWriterStateT.applyF(runF)
    }
}

abstract private[data] class RWSTFunctions extends CommonIRWSTConstructors {

  /**
   * Construct a new computation using the provided function.
   */
  def apply[F[_], E, L, S, A](
    runF: (E, S) => F[(L, S, A)]
  )(implicit F: Applicative[F]): ReaderWriterStateT[F, E, L, S, A] =
    new IndexedReaderWriterStateT(F.pure(runF))

  /**
   * Like [[apply]], but using a function in a context `F`.
   */
  def applyF[F[_], E, L, S, A](runF: F[(E, S) => F[(L, S, A)]]): ReaderWriterStateT[F, E, L, S, A] =
    new IndexedReaderWriterStateT(runF)

  /**
   * Like [[apply]], but using a effectful function from `S`.
   */
  def applyS[F[_]: Applicative, E, L, S, A](f: S => F[(L, S, A)]): IndexedReaderWriterStateT[F, E, L, S, S, A] =
    apply((_, s) => f(s))

  /**
   * Modify the input state using `f`.
   */
  def modify[F[_], E, L, S](f: S => S)(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, Unit] =
    ReaderWriterStateT((_, s) => F.pure((L.empty, f(s), ())))

  /**
   * Like [[modify]], but using an effectful function.
   */
  def modifyF[F[_], E, L, S](
    f: S => F[S]
  )(implicit F: Applicative[F], L: Monoid[L]): ReaderWriterStateT[F, E, L, S, Unit] =
    ReaderWriterStateT((_, s) => F.map(f(s))((L.empty, _, ())))

  def listen[F[_], E, L, S, A](rwst: ReaderWriterStateT[F, E, L, S, A])(implicit
    F: Functor[F]
  ): ReaderWriterStateT[F, E, L, S, (A, L)] = rwst.listen
}

/**
 * Convenience functions for ReaderWriterState.
 */
abstract private[data] class RWSFunctions {

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

  def listen[E, L, S, A](rws: ReaderWriterState[E, L, S, A]): ReaderWriterState[E, L, S, (A, L)] =
    rws.listen
}

sealed abstract private[data] class IRWSTInstances extends IRWSTInstances1 {

  implicit def catsDataStrongForIRWST[F[_], E, L, T](implicit
    F0: Monad[F]
  ): Strong[IndexedReaderWriterStateT[F, E, L, *, *, T]] =
    new IRWSTStrong[F, E, L, T] {
      implicit def F: Monad[F] = F0
    }

  implicit def catsDataBifunctorForIRWST[F[_], E, L, SA](implicit
    F0: Functor[F]
  ): Bifunctor[IndexedReaderWriterStateT[F, E, L, SA, *, *]] =
    new IRWSTBifunctor[F, E, L, SA] {
      implicit def F: Functor[F] = F0
    }

  implicit def catsDataContravariantForIRWST[F[_], E, L, SB, T](implicit
    F0: Functor[F]
  ): Contravariant[IndexedReaderWriterStateT[F, E, L, *, SB, T]] =
    new IRWSTContravariant[F, E, L, SB, T] {
      implicit def F: Functor[F] = F0
    }

  implicit def catsDataMonadErrorForIRWST[F[_], E, L, S, R](implicit
    F0: MonadError[F, R],
    L0: Monoid[L]
  ): MonadError[IndexedReaderWriterStateT[F, E, L, S, S, *], R] =
    new RWSTMonadError[F, E, L, S, R] {
      implicit def F: MonadError[F, R] = F0
      implicit def L: Monoid[L] = L0
    }

  implicit def catsDataDeferForIRWST[F[_], E, L, SA, SB](implicit
    F: Defer[F]
  ): Defer[IndexedReaderWriterStateT[F, E, L, SA, SB, *]] =
    new Defer[IndexedReaderWriterStateT[F, E, L, SA, SB, *]] {
      def defer[A](
        fa: => IndexedReaderWriterStateT[F, E, L, SA, SB, A]
      ): IndexedReaderWriterStateT[F, E, L, SA, SB, A] =
        IndexedReaderWriterStateT.applyF(F.defer(fa.runF))
    }

}

sealed abstract private[data] class IRWSTInstances1 extends IRWSTInstances2 {
  implicit def catsDataMonadForRWST[F[_], E, L, S](implicit
    F0: Monad[F],
    L0: Monoid[L]
  ): Monad[ReaderWriterStateT[F, E, L, S, *]] =
    new RWSTMonad[F, E, L, S] {
      implicit def F: Monad[F] = F0
      implicit def L: Monoid[L] = L0
    }

  implicit def catsDataProfunctorForIRWST[F[_], E, L, T](implicit
    F0: Functor[F]
  ): Profunctor[IndexedReaderWriterStateT[F, E, L, *, *, T]] =
    new IRWSTProfunctor[F, E, L, T] {
      implicit def F: Functor[F] = F0
    }

}

sealed abstract private[data] class IRWSTInstances2 extends IRWSTInstances3 {
  implicit def catsDataAlternativeForIRWST[F[_], E, L, S](implicit
    FM: Monad[F],
    FA: Alternative[F],
    L0: Monoid[L]
  ): Alternative[IndexedReaderWriterStateT[F, E, L, S, S, *]] =
    new RWSTAlternative[F, E, L, S] {
      implicit def G: Alternative[F] = FA
      implicit def F: Monad[F] = FM
      implicit def L: Monoid[L] = L0
    }
}

sealed abstract private[data] class IRWSTInstances3 {
  implicit def catsDataSemigroupKForIRWST[F[_], E, L, SA, SB](implicit
    F0: Monad[F],
    G0: SemigroupK[F]
  ): SemigroupK[IndexedReaderWriterStateT[F, E, L, SA, SB, *]] =
    new IRWSTSemigroupK[F, E, L, SA, SB] {
      implicit def F: Monad[F] = F0
      implicit def G: SemigroupK[F] = G0
    }

  implicit def catsDataFunctorForIRWST[F[_], E, L, SA, SB](implicit
    F0: Functor[F]
  ): Functor[IndexedReaderWriterStateT[F, E, L, SA, SB, *]] =
    new IRWSTFunctor[F, E, L, SA, SB] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract private[data] class IRWSTFunctor[F[_], E, L, SA, SB]
    extends Functor[IndexedReaderWriterStateT[F, E, L, SA, SB, *]] {
  implicit def F: Functor[F]

  override def map[A, B](
    fa: IndexedReaderWriterStateT[F, E, L, SA, SB, A]
  )(f: A => B): IndexedReaderWriterStateT[F, E, L, SA, SB, B] =
    fa.map(f)
}

sealed abstract private[data] class IRWSTContravariant[F[_], E, L, SB, T]
    extends Contravariant[IndexedReaderWriterStateT[F, E, L, *, SB, T]] {
  implicit def F: Functor[F]

  override def contramap[A, B](
    fa: IndexedReaderWriterStateT[F, E, L, A, SB, T]
  )(f: B => A): IndexedReaderWriterStateT[F, E, L, B, SB, T] =
    fa.contramap(f)
}

sealed abstract private[data] class IRWSTProfunctor[F[_], E, L, T]
    extends Profunctor[IndexedReaderWriterStateT[F, E, L, *, *, T]] {
  implicit def F: Functor[F]

  override def dimap[A, B, C, D](
    fab: IndexedReaderWriterStateT[F, E, L, A, B, T]
  )(f: C => A)(g: B => D): IndexedReaderWriterStateT[F, E, L, C, D, T] =
    fab.dimap(f)(g)
}

sealed abstract private[data] class IRWSTStrong[F[_], E, L, T]
    extends IRWSTProfunctor[F, E, L, T]
    with Strong[IndexedReaderWriterStateT[F, E, L, *, *, T]] {
  implicit def F: Monad[F]

  def first[A, B, C](
    fa: IndexedReaderWriterStateT[F, E, L, A, B, T]
  ): IndexedReaderWriterStateT[F, E, L, (A, C), (B, C), T] =
    IndexedReaderWriterStateT { case (e, (a, c)) =>
      F.map(fa.run(e, a)) { case (l, b, t) =>
        (l, (b, c), t)
      }
    }

  def second[A, B, C](
    fa: IndexedReaderWriterStateT[F, E, L, A, B, T]
  ): IndexedReaderWriterStateT[F, E, L, (C, A), (C, B), T] =
    first(fa).dimap((_: (C, A)).swap)(_.swap)
}

sealed abstract private[data] class IRWSTBifunctor[F[_], E, L, SA]
    extends Bifunctor[IndexedReaderWriterStateT[F, E, L, SA, *, *]] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](
    fab: IndexedReaderWriterStateT[F, E, L, SA, A, B]
  )(f: A => C, g: B => D): IndexedReaderWriterStateT[F, E, L, SA, C, D] =
    fab.bimap(f, g)
}

sealed abstract private[data] class RWSTMonad[F[_], E, L, S]
    extends IRWSTFunctor[F, E, L, S, S]
    with Monad[ReaderWriterStateT[F, E, L, S, *]] {
  implicit def F: Monad[F]
  implicit def L: Monoid[L]

  def pure[A](a: A): ReaderWriterStateT[F, E, L, S, A] =
    ReaderWriterStateT.pure(a)

  def flatMap[A, B](
    fa: ReaderWriterStateT[F, E, L, S, A]
  )(f: A => ReaderWriterStateT[F, E, L, S, B]): ReaderWriterStateT[F, E, L, S, B] =
    fa.flatMap(f)

  def tailRecM[A, B](
    initA: A
  )(f: A => ReaderWriterStateT[F, E, L, S, Either[A, B]]): ReaderWriterStateT[F, E, L, S, B] =
    ReaderWriterStateT { (e, initS) =>
      F.tailRecM((L.empty, initS, initA)) { case (currL, currS, currA) =>
        F.map(f(currA).run(e, currS)) { case (nextL, nextS, ab) =>
          ab match {
            case Right(b) => Right((L.combine(currL, nextL), nextS, b))
            case Left(a)  => Left((L.combine(currL, nextL), nextS, a))
          }
        }
      }
    }
}

sealed abstract private[data] class IRWSTSemigroupK[F[_], E, L, SA, SB] extends IRWSTSemigroupK1[F, E, L, SA, SB]

sealed abstract private[data] class RWSTAlternative[F[_], E, L, S]
    extends IRWSTFunctor[F, E, L, S, S]
    with RWSTAlternative1[F, E, L, S]

sealed abstract private[data] class RWSTMonadError[F[_], E, L, S, R]
    extends RWSTMonad[F, E, L, S]
    with MonadError[ReaderWriterStateT[F, E, L, S, *], R] {

  implicit def F: MonadError[F, R]

  def raiseError[A](r: R): ReaderWriterStateT[F, E, L, S, A] = ReaderWriterStateT.liftF(F.raiseError(r))

  def handleErrorWith[A](
    fa: ReaderWriterStateT[F, E, L, S, A]
  )(f: R => ReaderWriterStateT[F, E, L, S, A]): ReaderWriterStateT[F, E, L, S, A] =
    ReaderWriterStateT { (e, s) =>
      F.handleErrorWith(fa.run(e, s))(r => f(r).run(e, s))
    }
}

private trait IRWSTSemigroupK1[F[_], E, L, SA, SB] extends SemigroupK[IndexedReaderWriterStateT[F, E, L, SA, SB, *]] {
  implicit def F: Monad[F]
  implicit def G: SemigroupK[F]

  def combineK[A](x: IndexedReaderWriterStateT[F, E, L, SA, SB, A],
                  y: IndexedReaderWriterStateT[F, E, L, SA, SB, A]
  ): IndexedReaderWriterStateT[F, E, L, SA, SB, A] =
    IndexedReaderWriterStateT { (e, sa) =>
      G.combineK(x.run(e, sa), y.run(e, sa))
    }
}

private trait RWSTAlternative1[F[_], E, L, S]
    extends IRWSTSemigroupK1[F, E, L, S, S]
    with Alternative[ReaderWriterStateT[F, E, L, S, *]] {

  implicit def F: Monad[F]
  def G: Alternative[F]
  implicit def L: Monoid[L]

  def empty[A]: ReaderWriterStateT[F, E, L, S, A] = ReaderWriterStateT.liftF(G.empty[A])

  def pure[A](a: A): ReaderWriterStateT[F, E, L, S, A] = ReaderWriterStateT.pure[F, E, L, S, A](a)

  def ap[A, B](
    ff: ReaderWriterStateT[F, E, L, S, A => B]
  )(fa: ReaderWriterStateT[F, E, L, S, A]): ReaderWriterStateT[F, E, L, S, B] =
    ff.flatMap(f => fa.map(f)(F))(F, L)

}
