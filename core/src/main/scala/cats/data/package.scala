package cats

package object data {
  type NonEmptyStream[A] = OneAnd[Stream, A]
  type ValidatedNel[+E, +A] = Validated[NonEmptyList[E], A]
  type IorNel[+B, +A] = Ior[NonEmptyList[B], A]
  type EitherNel[+E, +A] = Either[NonEmptyList[E], A]

  def NonEmptyStream[A](head: A, tail: Stream[A] = Stream.empty): NonEmptyStream[A] =
    OneAnd(head, tail)
  def NonEmptyStream[A](head: A, tail: A*): NonEmptyStream[A] =
    OneAnd(head, tail.toStream)

  type ReaderT[F[_], A, B] = Kleisli[F, A, B]
  val ReaderT = Kleisli

  type Reader[A, B] = ReaderT[Id, A, B]

  object Reader {
    def apply[A, B](f: A => B): Reader[A, B] = ReaderT[Id, A, B](f)
  }

  type Writer[L, V] = WriterT[Id, L, V]
  object Writer {
    def apply[L, V](l: L, v: V): WriterT[Id, L, V] = WriterT[Id, L, V]((l, v))

    def value[L:Monoid, V](v: V): Writer[L, V] = WriterT.value(v)

    def tell[L](l: L): Writer[L, Unit] = WriterT.tell(l)
  }

  /**
   * `StateT[F, S, A]` is similar to `Kleisli[F, S, A]` in that it takes an `S`
   * argument and produces an `A` value wrapped in `F`. However, it also produces
   * an `S` value representing the updated state (which is wrapped in the `F`
   * context along with the `A` value.
   */
  type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
  object StateT extends StateTFunctions

  type State[S, A] = StateT[Eval, S, A]
  object State extends StateFunctions

  type RWST[F[_], E, S, L, A] = ReaderWriterStateT[F, E, S, L, A]
  val RWST = ReaderWriterStateT

  type ReaderWriterState[E, L, S, A] = ReaderWriterStateT[Eval, E, L, S, A]
  object ReaderWriterState extends RWSFunctions

  type RWS[E, L, S, A] = ReaderWriterState[E, L, S, A]
  val RWS = ReaderWriterState
}
