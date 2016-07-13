package cats

package object data {
  type NonEmptyVector[A] = OneAnd[Vector, A]
  type NonEmptyStream[A] = OneAnd[Stream, A]
  type ValidatedNel[E, A] = Validated[NonEmptyList[E], A]

  def NonEmptyVector[A](head: A, tail: Vector[A] = Vector.empty): NonEmptyVector[A] =
    OneAnd(head, tail)
  def NonEmptyVector[A](head: A, tail: A*): NonEmptyVector[A] =
    OneAnd(head, tail.toVector)

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
  }

  type State[S, A] = StateT[Eval, S, A]
  object State extends StateFunctions
}
