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

package object data extends ScalaVersionSpecificPackage {

  type ValidatedNel[+E, +A] = Validated[NonEmptyList[E], A]
  type ValidatedNec[+E, +A] = Validated[NonEmptyChain[E], A]
  type IorNel[+B, +A] = Ior[NonEmptyList[B], A]
  type IorNec[+B, +A] = Ior[NonEmptyChain[B], A]
  type IorNes[B, +A] = Ior[NonEmptySet[B], A]
  type EitherNel[+E, +A] = Either[NonEmptyList[E], A]
  type EitherNec[+E, +A] = Either[NonEmptyChain[E], A]
  type EitherNes[E, +A] = Either[NonEmptySet[E], A]

  type NonEmptyMap[K, +A] = NonEmptyMapImpl.Type[K, A]
  val NonEmptyMap = NonEmptyMapImpl

  type NonEmptySet[A] = NonEmptySetImpl.Type[A]
  val NonEmptySet = NonEmptySetImpl

  type NonEmptyChain[+A] = NonEmptyChainImpl.Type[A]
  val NonEmptyChain = NonEmptyChainImpl

  type ReaderT[F[_], -A, B] = Kleisli[F, A, B]
  val ReaderT = Kleisli

  type Reader[-A, B] = ReaderT[Id, A, B]

  object Reader {
    def apply[A, B](f: A => B): Reader[A, B] = ReaderT[Id, A, B](f)

    def local[A, R](f: R => R)(fa: Reader[R, A]): Reader[R, A] = Kleisli.local(f)(fa)
  }

  type Writer[L, V] = WriterT[Id, L, V]
  object Writer {
    def apply[L, V](l: L, v: V): WriterT[Id, L, V] = WriterT[Id, L, V]((l, v))

    def value[L: Monoid, V](v: V): Writer[L, V] = WriterT.value(v)

    def tell[L](l: L): Writer[L, Unit] = WriterT.tell(l)

    def listen[L, V](writer: Writer[L, V]): Writer[L, (V, L)] =
      WriterT.listen(writer)
  }

  type IndexedState[S1, S2, A] = IndexedStateT[Eval, S1, S2, A]
  object IndexedState extends IndexedStateFunctions

  /**
   * `StateT[F, S, A]` is similar to `Kleisli[F, S, A]` in that it takes an `S`
   * argument and produces an `A` value wrapped in `F`. However, it also produces
   * an `S` value representing the updated state (which is wrapped in the `F`
   * context along with the `A` value.
   */
  type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
  object StateT extends StateTFunctions with CommonStateTConstructors0

  type State[S, A] = StateT[Eval, S, A]
  object State extends StateFunctions

  type IRWST[F[_], E, L, SA, SB, A] = IndexedReaderWriterStateT[F, E, L, SA, SB, A]
  val IRWST = IndexedReaderWriterStateT

  /**
   * Represents a stateful computation in a context `F[_]`, over state `S`, with an
   * initial environment `E`, an accumulated log `L` and a result `A`.
   */
  type ReaderWriterStateT[F[_], E, L, S, A] = IndexedReaderWriterStateT[F, E, L, S, S, A]
  object ReaderWriterStateT extends RWSTFunctions

  type RWST[F[_], E, L, S, A] = ReaderWriterStateT[F, E, L, S, A]
  val RWST = ReaderWriterStateT

  type ReaderWriterState[E, L, S, A] = ReaderWriterStateT[Eval, E, L, S, A]
  object ReaderWriterState extends RWSFunctions

  type RWS[E, L, S, A] = ReaderWriterState[E, L, S, A]
  val RWS = ReaderWriterState

  type Store[S, A] = RepresentableStore[S => *, S, A]
  object Store {
    def apply[S, A](f: S => A, s: S): Store[S, A] =
      RepresentableStore[S => *, S, A](f, s)
  }

  type Cont[A, B] = ContT[Eval, A, B]

  object Cont {
    def apply[A, B](f: (B => Eval[A]) => Eval[A]): Cont[A, B] =
      ContT[Eval, A, B](f)

    def pure[A, B](b: B): Cont[A, B] =
      ContT.pure[Eval, A, B](b)

    def defer[A, B](b: => B): Cont[A, B] =
      ContT.defer[Eval, A, B](b)

    def later[A, B](fn: => (B => Eval[A]) => Eval[A]): Cont[A, B] =
      ContT.later(fn)

    def tailRecM[A, B, C](a: A)(f: A => Cont[C, Either[A, B]]): Cont[C, B] =
      ContT.tailRecM(a)(f)

    def liftF[A, B](b: Eval[B]): Cont[A, B] = ContT.liftF(b)

    def callCC[A, B, C](f: (B => Cont[A, C]) => Cont[A, B]): Cont[A, B] =
      ContT.callCC(f)

    def reset[A, B](cont: Cont[A, A]): Cont[B, A] =
      ContT.resetT(cont)

    def shift[A, B](f: (B => Eval[A]) => Cont[A, A]): Cont[A, B] =
      ContT.shiftT(f)
  }

  type StoreT[W[_], S, A] = RepresentableStoreT[W, Function1[S, *], S, A]

  object StoreT {

    def pure[W[_], S, A](x: A)(implicit W: Applicative[W], S: Monoid[S]): StoreT[W, S, A] =
      RepresentableStoreT.pure[W, Function1[S, *], S, A](x)

  }
}
