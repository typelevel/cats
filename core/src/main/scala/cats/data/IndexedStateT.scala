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
 * `IndexedStateT[F, SA, SB, A]` is a stateful computation in a context `F` yielding
 * a value of type `A`. The state transitions from a value of type `SA` to a value
 * of type `SB`.
 *
 * Note that for the `SA != SB` case, this is an indexed monad. Indexed monads
 * are monadic type constructors annotated by an additional type for effect
 * tracking purposes. In this case, the annotation tracks the initial state and
 * the resulting state.
 *
 * Given `IndexedStateT[F, S, S, A]`, this yields the `StateT[F, S, A]` monad.
 */
final class IndexedStateT[F[_], SA, SB, A](val runF: F[SA => F[(SB, A)]]) extends Serializable {

  def flatMap[B, SC](fas: A => IndexedStateT[F, SB, SC, B])(implicit F: FlatMap[F]): IndexedStateT[F, SA, SC, B] =
    IndexedStateT.applyF(F.map(runF) { safsba =>
      AndThen(safsba).andThen { fsba =>
        F.flatMap(fsba) { case (sb, a) =>
          fas(a).run(sb)
        }
      }
    })

  def flatMapF[B](faf: A => F[B])(implicit F: FlatMap[F]): IndexedStateT[F, SA, SB, B] =
    IndexedStateT.applyF(F.map(runF) { sfsa =>
      AndThen(sfsa).andThen { fsa =>
        F.flatMap(fsa) { case (s, a) => F.map(faf(a))((s, _)) }
      }
    })

  def map[B](f: A => B)(implicit F: Functor[F]): IndexedStateT[F, SA, SB, B] =
    transform { case (s, a) => (s, f(a)) }

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): IndexedStateT[G, SA, SB, A] =
    IndexedStateT.applyF(f(F.map(runF)(_.andThen(fsa => f(fsa)))))

  def contramap[S0](f: S0 => SA)(implicit F: Functor[F]): IndexedStateT[F, S0, SB, A] =
    IndexedStateT.applyF {
      F.map(runF) { safsba => (s0: S0) =>
        safsba(f(s0))
      }
    }

  def bimap[SC, B](f: SB => SC, g: A => B)(implicit F: Functor[F]): IndexedStateT[F, SA, SC, B] =
    transform { (s, a) =>
      (f(s), g(a))
    }

  def dimap[S0, S1](f: S0 => SA)(g: SB => S1)(implicit F: Functor[F]): IndexedStateT[F, S0, S1, A] =
    contramap(f).modify(g)

  /**
   * Run with the provided initial state value
   */
  def run(initial: SA)(implicit F: FlatMap[F]): F[(SB, A)] =
    F.flatMap(runF)(f => f(initial))

  /**
   * Run with the provided initial state value and return the final state
   * (discarding the final value).
   */
  def runS(s: SA)(implicit F: FlatMap[F]): F[SB] = F.map(run(s))(_._1)

  /**
   * Run with the provided initial state value and return the final value
   * (discarding the final state).
   */
  def runA(s: SA)(implicit F: FlatMap[F]): F[A] = F.map(run(s))(_._2)

  /**
   * Run with `S`'s empty monoid value as the initial state.
   */
  def runEmpty(implicit S: Monoid[SA], F: FlatMap[F]): F[(SB, A)] = run(S.empty)

  /**
   * Run with `S`'s empty monoid value as the initial state and return the final
   * state (discarding the final value).
   */
  def runEmptyS(implicit S: Monoid[SA], F: FlatMap[F]): F[SB] = runS(S.empty)

  /**
   * Run with `S`'s empty monoid value as the initial state and return the final
   * value (discarding the final state).
   */
  def runEmptyA(implicit S: Monoid[SA], F: FlatMap[F]): F[A] = runA(S.empty)

  /**
   * Like [[map]], but also allows the state (`S`) value to be modified.
   */
  def transform[B, SC](f: (SB, A) => (SC, B))(implicit F: Functor[F]): IndexedStateT[F, SA, SC, B] =
    IndexedStateT.applyF(F.map(runF) { sfsa =>
      AndThen(sfsa).andThen { fsa =>
        F.map(fsa) { case (s, a) => f(s, a) }
      }
    })

  /**
   * Like [[transform]], but allows the context to change from `F` to `G`.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> type ErrorOr[A] = Either[String, A]
   * scala> val xError: IndexedStateT[ErrorOr, Int, Int, Int] = IndexedStateT.get
   * scala> val xOpt: IndexedStateT[Option, Int, Int, Int] = xError.transformF(_.toOption)
   * scala> val input = 5
   * scala> xError.run(input)
   * res0: ErrorOr[(Int, Int)] = Right((5,5))
   * scala> xOpt.run(5)
   * res1: Option[(Int, Int)] = Some((5,5))
   * }}}
   */
  def transformF[G[_], B, SC](
    f: F[(SB, A)] => G[(SC, B)]
  )(implicit F: FlatMap[F], G: Applicative[G]): IndexedStateT[G, SA, SC, B] =
    IndexedStateT(s => f(run(s)))

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
  def transformS[R](f: R => SA, g: (R, SB) => R)(implicit F: Functor[F]): IndexedStateT[F, R, R, A] =
    StateT.applyF(F.map(runF) { sfsa => (r: R) =>
      val sa = f(r)
      val fsba = sfsa(sa)
      F.map(fsba) { case (sb, a) => (g(r, sb), a) }
    })

  /**
   * Modify the state (`S`) component.
   */
  def modify[SC](f: SB => SC)(implicit F: Functor[F]): IndexedStateT[F, SA, SC, A] =
    transform((s, a) => (f(s), a))

  /**
   * Inspect a value from the input state, without modifying the state.
   */
  def inspect[B](f: SB => B)(implicit F: Functor[F]): IndexedStateT[F, SA, SB, B] =
    transform((s, _) => (s, f(s)))

  /**
   * Get the input state, without modifying the state.
   */
  def get(implicit F: Functor[F]): IndexedStateT[F, SA, SB, SB] =
    inspect(identity)
}

private[data] trait CommonStateTConstructors {
  def pure[F[_], S, A](a: A)(implicit F: Applicative[F]): IndexedStateT[F, S, S, A] =
    IndexedStateT(s => F.pure((s, a)))

  def liftF[F[_], S, A](fa: F[A])(implicit F: Applicative[F]): IndexedStateT[F, S, S, A] =
    IndexedStateT(s => F.map(fa)(a => (s, a)))

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, *]]
   * scala> val b: OptionT[StateT[Eval, String, *], Int] = a.mapK(StateT.liftK)
   * scala> b.value.runEmpty.value
   * res0: (String, Option[Int]) = ("",Some(1))
   * }}}
   */
  def liftK[F[_], S](implicit F: Applicative[F]): F ~> IndexedStateT[F, S, S, *] =
    new (F ~> IndexedStateT[F, S, S, *]) { def apply[A](a: F[A]): IndexedStateT[F, S, S, A] = IndexedStateT.liftF(a) }

  @deprecated("Use liftF instead", "1.0.0-RC2")
  def lift[F[_], S, A](fa: F[A])(implicit F: Applicative[F]): IndexedStateT[F, S, S, A] =
    IndexedStateT(s => F.map(fa)(a => (s, a)))

  def inspect[F[_], S, A](f: S => A)(implicit F: Applicative[F]): IndexedStateT[F, S, S, A] =
    IndexedStateT(s => F.pure((s, f(s))))

  def inspectF[F[_], S, A](f: S => F[A])(implicit F: Applicative[F]): IndexedStateT[F, S, S, A] =
    IndexedStateT(s => F.map(f(s))(a => (s, a)))

  def get[F[_], S](implicit F: Applicative[F]): IndexedStateT[F, S, S, S] =
    IndexedStateT(s => F.pure((s, s)))

  /**
   * Turn `State[A, F[B]]` into `StateT[F, A, B]`
   */
  def fromState[F[_], A, B](s: State[A, F[B]])(implicit F: Applicative[F]): StateT[F, A, B] =
    s.transformF { eval =>
      val (a, fb) = eval.value
      F.map(fb)((a, _))
    }
}

object IndexedStateT extends IndexedStateTInstances with CommonStateTConstructors0 {
  def apply[F[_], SA, SB, A](f: SA => F[(SB, A)])(implicit F: Applicative[F]): IndexedStateT[F, SA, SB, A] =
    new IndexedStateT(F.pure(f))

  def applyF[F[_], SA, SB, A](runF: F[SA => F[(SB, A)]]): IndexedStateT[F, SA, SB, A] =
    new IndexedStateT(runF)

  def modify[F[_], SA, SB](f: SA => SB)(implicit F: Applicative[F]): IndexedStateT[F, SA, SB, Unit] =
    IndexedStateT(sa => F.pure((f(sa), ())))

  def modifyF[F[_], SA, SB](f: SA => F[SB])(implicit F: Applicative[F]): IndexedStateT[F, SA, SB, Unit] =
    IndexedStateT(s => F.map(f(s))(s => (s, ())))

  def set[F[_], SA, SB](sb: SB)(implicit F: Applicative[F]): IndexedStateT[F, SA, SB, Unit] =
    IndexedStateT(_ => F.pure((sb, ())))

  def setF[F[_], SA, SB](fsb: F[SB])(implicit F: Applicative[F]): IndexedStateT[F, SA, SB, Unit] =
    IndexedStateT(_ => F.map(fsb)(s => (s, ())))
}

private[data] trait CommonStateTConstructors0 extends CommonStateTConstructors {
  def empty[F[_], S, A](implicit A: Monoid[A], F: Applicative[F]): IndexedStateT[F, S, S, A] =
    pure(A.empty)
}

abstract private[data] class StateTFunctions extends CommonStateTConstructors {
  def apply[F[_], S, A](f: S => F[(S, A)])(implicit F: Applicative[F]): StateT[F, S, A] =
    IndexedStateT(f)

  def applyF[F[_], S, A](runF: F[S => F[(S, A)]]): StateT[F, S, A] =
    IndexedStateT.applyF(runF)

  def modify[F[_], S](f: S => S)(implicit F: Applicative[F]): StateT[F, S, Unit] =
    apply(sa => F.pure((f(sa), ())))

  def modifyF[F[_], S](f: S => F[S])(implicit F: Applicative[F]): StateT[F, S, Unit] =
    apply(s => F.map(f(s))(s => (s, ())))

  def set[F[_], S](s: S)(implicit F: Applicative[F]): StateT[F, S, Unit] =
    apply(_ => F.pure((s, ())))

  def setF[F[_], S](fs: F[S])(implicit F: Applicative[F]): StateT[F, S, Unit] =
    apply(_ => F.map(fs)(s => (s, ())))
}

sealed abstract private[data] class IndexedStateTInstances extends IndexedStateTInstances1 {
  implicit def catsDataAlternativeForIndexedStateT[F[_], S](implicit
    FM: Monad[F],
    FA: Alternative[F]
  ): Alternative[IndexedStateT[F, S, S, *]] with Monad[IndexedStateT[F, S, S, *]] =
    new IndexedStateTAlternative[F, S] { implicit def F = FM; implicit def G = FA }

  implicit def catsDataDeferForIndexedStateT[F[_], SA, SB](implicit F: Defer[F]): Defer[IndexedStateT[F, SA, SB, *]] =
    new Defer[IndexedStateT[F, SA, SB, *]] {
      def defer[A](fa: => IndexedStateT[F, SA, SB, A]): IndexedStateT[F, SA, SB, A] =
        IndexedStateT.applyF(F.defer(fa.runF))
    }

  implicit def catsDataFunctorFilterForIndexedStateT[F[_], SA, SB](implicit
    ev1: Monad[F],
    ev2: FunctorFilter[F]
  ): FunctorFilter[IndexedStateT[F, SA, SB, *]] =
    new IndexedStateTFunctorFilter[F, SA, SB] {
      val F0 = ev1
      val FF = ev2
    }
}

sealed abstract private[data] class IndexedStateTInstances1 extends IndexedStateTInstances2 {
  implicit def catsDataMonadErrorForIndexedStateT[F[_], S, E](implicit
    F0: MonadError[F, E]
  ): MonadError[IndexedStateT[F, S, S, *], E] =
    new IndexedStateTMonadError[F, S, E] { implicit def F = F0 }

  implicit def catsDataSemigroupKForIndexedStateT[F[_], SA, SB](implicit
    F0: Monad[F],
    G0: SemigroupK[F]
  ): SemigroupK[IndexedStateT[F, SA, SB, *]] =
    new IndexedStateTSemigroupK[F, SA, SB] { implicit def F = F0; implicit def G = G0 }
}

sealed abstract private[data] class IndexedStateTInstances2 extends IndexedStateTInstances3 {
  implicit def catsDataMonadForIndexedStateT[F[_], S](implicit F0: Monad[F]): Monad[IndexedStateT[F, S, S, *]] =
    new IndexedStateTMonad[F, S] { implicit def F = F0 }
}

sealed abstract private[data] class IndexedStateTInstances3 extends IndexedStateTInstances4 {
  implicit def catsDataFunctorForIndexedStateT[F[_], SA, SB](implicit
    F0: Functor[F]
  ): Functor[IndexedStateT[F, SA, SB, *]] =
    new IndexedStateTFunctor[F, SA, SB] { implicit def F = F0 }

  implicit def catsDataContravariantForIndexedStateT[F[_], SB, V](implicit
    F0: Functor[F]
  ): Contravariant[IndexedStateT[F, *, SB, V]] =
    new IndexedStateTContravariant[F, SB, V] { implicit def F = F0 }

  implicit def catsDataProfunctorForIndexedStateT[F[_], V](implicit
    F0: Functor[F]
  ): Profunctor[IndexedStateT[F, *, *, V]] =
    new IndexedStateTProfunctor[F, V] { implicit def F = F0 }

  implicit def catsDataBifunctorForIndexedStateT[F[_], SA](implicit
    F0: Functor[F]
  ): Bifunctor[IndexedStateT[F, SA, *, *]] =
    new IndexedStateTBifunctor[F, SA] { implicit def F = F0 }
}

sealed abstract private[data] class IndexedStateTInstances4 {
  implicit def catsDataStrongForIndexedStateT[F[_], V](implicit F0: Monad[F]): Strong[IndexedStateT[F, *, *, V]] =
    new IndexedStateTStrong[F, V] { implicit def F = F0 }
}

abstract private[data] class IndexedStateFunctions {

  /**
   * Instantiate an `IndexedState[S1, S2, A]`.
   *
   * Example:
   * {{{
   * scala> import cats.data.IndexedState
   *
   * scala> val is = IndexedState[Int, Long, String](i => (i + 1L, "Here is " + i))
   * scala> is.run(3).value
   * res0: (Long, String) = (4,Here is 3)
   * }}}
   */
  def apply[S1, S2, A](f: S1 => (S2, A)): IndexedState[S1, S2, A] =
    IndexedStateT.applyF(Now((s: S1) => Now(f(s))))
}

// To workaround SI-7139 `object State` needs to be defined inside the package object
// together with the type alias.
abstract private[data] class StateFunctions {

  def apply[S, A](f: S => (S, A)): State[S, A] =
    IndexedStateT.applyF(Now((s: S) => Now(f(s))))

  /**
   * Return `a` and maintain the input state.
   */
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))

  /**
   * Return `A`'s empty monoid value and maintain the input state.
   */
  def empty[S, A](implicit A: Monoid[A]): State[S, A] = pure(A.empty)

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

sealed abstract private[data] class IndexedStateTFunctor[F[_], SA, SB] extends Functor[IndexedStateT[F, SA, SB, *]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: IndexedStateT[F, SA, SB, A])(f: A => B): IndexedStateT[F, SA, SB, B] =
    fa.map(f)
}

sealed abstract private[data] class IndexedStateTContravariant[F[_], SB, V]
    extends Contravariant[IndexedStateT[F, *, SB, V]] {
  implicit def F: Functor[F]

  override def contramap[A, B](fa: IndexedStateT[F, A, SB, V])(f: B => A): IndexedStateT[F, B, SB, V] =
    fa.contramap(f)
}

sealed abstract private[data] class IndexedStateTBifunctor[F[_], SA] extends Bifunctor[IndexedStateT[F, SA, *, *]] {
  implicit def F: Functor[F]

  def bimap[A, B, C, D](fab: IndexedStateT[F, SA, A, B])(f: A => C, g: B => D): IndexedStateT[F, SA, C, D] =
    fab.bimap(f, g)
}

sealed abstract private[data] class IndexedStateTProfunctor[F[_], V] extends Profunctor[IndexedStateT[F, *, *, V]] {
  implicit def F: Functor[F]

  def dimap[A, B, C, D](fab: IndexedStateT[F, A, B, V])(f: C => A)(g: B => D): IndexedStateT[F, C, D, V] =
    fab.dimap(f)(g)
}

sealed abstract private[data] class IndexedStateTStrong[F[_], V]
    extends IndexedStateTProfunctor[F, V]
    with Strong[IndexedStateT[F, *, *, V]] {
  implicit def F: Monad[F]

  def first[A, B, C](fa: IndexedStateT[F, A, B, V]): IndexedStateT[F, (A, C), (B, C), V] =
    IndexedStateT { case (a, c) =>
      F.map(fa.run(a)) { case (b, v) =>
        ((b, c), v)
      }
    }

  def second[A, B, C](fa: IndexedStateT[F, A, B, V]): IndexedStateT[F, (C, A), (C, B), V] =
    first(fa).dimap((_: (C, A)).swap)(_.swap)
}

sealed abstract private[data] class IndexedStateTMonad[F[_], S]
    extends IndexedStateTFunctor[F, S, S]
    with Monad[IndexedStateT[F, S, S, *]] {
  implicit def F: Monad[F]

  def pure[A](a: A): IndexedStateT[F, S, S, A] =
    IndexedStateT.pure(a)

  def flatMap[A, B](fa: IndexedStateT[F, S, S, A])(f: A => IndexedStateT[F, S, S, B]): IndexedStateT[F, S, S, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(f: A => IndexedStateT[F, S, S, Either[A, B]]): IndexedStateT[F, S, S, B] =
    IndexedStateT[F, S, S, B](s =>
      F.tailRecM[(S, A), (S, B)]((s, a)) { case (s, a) =>
        F.map(f(a).run(s)) { case (s, ab) =>
          ab match {
            case Right(b) => Right((s, b))
            case Left(a)  => Left((s, a))
          }
        }
      }
    )
}

sealed abstract private[data] class IndexedStateTSemigroupK[F[_], SA, SB]
    extends SemigroupK[IndexedStateT[F, SA, SB, *]] {
  implicit def F: Monad[F]
  implicit def G: SemigroupK[F]

  def combineK[A](x: IndexedStateT[F, SA, SB, A], y: IndexedStateT[F, SA, SB, A]): IndexedStateT[F, SA, SB, A] =
    IndexedStateT(s => G.combineK(x.run(s), y.run(s)))
}

sealed abstract private[data] class IndexedStateTContravariantMonoidal[F[_], S]
    extends ContravariantMonoidal[IndexedStateT[F, S, S, *]] {
  implicit def F: ContravariantMonoidal[F]
  implicit def G: Applicative[F]

  override val unit: IndexedStateT[F, S, S, Unit] =
    IndexedStateT.applyF(G.pure((_: S) => F.trivial[(S, Unit)]))

  override def contramap[A, B](fa: IndexedStateT[F, S, S, A])(f: B => A): IndexedStateT[F, S, S, B] =
    contramap2(fa, trivial)(((a: A) => (a, a)).compose(f))

  override def product[A, B](fa: IndexedStateT[F, S, S, A],
                             fb: IndexedStateT[F, S, S, B]
  ): IndexedStateT[F, S, S, (A, B)] =
    contramap2(fa, fb)(identity)

  def contramap2[A, B, C](fb: IndexedStateT[F, S, S, B], fc: IndexedStateT[F, S, S, C])(
    f: A => (B, C)
  ): IndexedStateT[F, S, S, A] =
    IndexedStateT.applyF(
      G.pure((s: S) =>
        ContravariantMonoidal.contramap2(G.map(fb.runF)(_.apply(s)), G.map(fc.runF)(_.apply(s)))((tup: (S, A)) =>
          f(tup._2) match {
            case (b, c) => (G.pure((tup._1, b)), G.pure((tup._1, c)))
          }
        )(G, F)
      )
    )
}

sealed abstract private[data] class IndexedStateTAlternative[F[_], S]
    extends IndexedStateTMonad[F, S]
    with Alternative[IndexedStateT[F, S, S, *]] {
  def G: Alternative[F]

  def combineK[A](x: IndexedStateT[F, S, S, A], y: IndexedStateT[F, S, S, A]): IndexedStateT[F, S, S, A] =
    IndexedStateT[F, S, S, A](s => G.combineK(x.run(s), y.run(s)))(G)

  def empty[A]: IndexedStateT[F, S, S, A] =
    IndexedStateT.liftF[F, S, A](G.empty[A])(G)
}

sealed abstract private[data] class IndexedStateTMonadError[F[_], S, E]
    extends IndexedStateTMonad[F, S]
    with MonadError[IndexedStateT[F, S, S, *], E] {
  implicit def F: MonadError[F, E]

  def raiseError[A](e: E): IndexedStateT[F, S, S, A] = IndexedStateT.liftF(F.raiseError(e))

  def handleErrorWith[A](fa: IndexedStateT[F, S, S, A])(f: E => IndexedStateT[F, S, S, A]): IndexedStateT[F, S, S, A] =
    IndexedStateT(s => F.handleErrorWith(fa.run(s))(e => f(e).run(s)))
}

private[this] trait IndexedStateTFunctorFilter[F[_], SA, SB] extends FunctorFilter[IndexedStateT[F, SA, SB, *]] {

  implicit def F0: Monad[F]
  def FF: FunctorFilter[F]

  def functor: Functor[IndexedStateT[F, SA, SB, *]] =
    IndexedStateT.catsDataFunctorForIndexedStateT(FF.functor)

  def mapFilter[A, B](fa: IndexedStateT[F, SA, SB, A])(f: A => Option[B]): IndexedStateT[F, SA, SB, B] =
    fa.flatMapF(a => FF.mapFilter(F0.pure(a))(f))
}
