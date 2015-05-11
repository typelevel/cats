package cats

package object data {
  type NonEmptyList[A] = OneAnd[A, List]
  type NonEmptyVector[A] = OneAnd[A, Vector]
  type NonEmptyStream[A] = OneAnd[A, Stream]

  def NonEmptyList[A](head: A, tail: List[A] = Nil) = OneAnd[A, List](head, tail)
  def NonEmptyList[A](head: A, tail: A*) = OneAnd[A, List](head, tail.toList)

  def NonEmptyVector[A](head: A, tail: Vector[A] = Vector.empty) = OneAnd[A, Vector](head, tail)
  def NonEmptyVector[A](head: A, tail: A*) = OneAnd[A, Vector](head, tail.toVector)

  def NonEmptyStream[A](head: A, tail: Stream[A] = Stream.empty) = OneAnd[A, Stream](head, tail)
  def NonEmptyStream[A](head: A, tail: A*) = OneAnd[A, Stream](head, tail.toStream)

  object NonEmptyList {
    def fromReducible[F[_], A](fa: F[A])(implicit F: Reducible[F]): Lazy[NonEmptyList[A]] =
      F.reduceRightTo(fa)(a => NonEmptyList(a, Nil)) { a =>
        Fold.Continue { case OneAnd(h, t) => OneAnd(a, h :: t) }
      }
  }
}
