package cats

package object data {
  type NonEmptyList[A] = OneAnd[A, List]
  type NonEmptyVector[A] = OneAnd[A, Vector]
  type NonEmptyStream[A] = OneAnd[A, Stream]

  def NonEmptyList[A](head: A, tail: List[A] = Nil): NonEmptyList[A] = OneAnd(head, tail)
  def NonEmptyList[A](head: A, tail: A*): NonEmptyList[A] = OneAnd[A, List](head, tail.toList)

  def NonEmptyVector[A](head: A, tail: Vector[A] = Vector.empty): NonEmptyVector[A] = OneAnd(head, tail)
  def NonEmptyVector[A](head: A, tail: A*): NonEmptyVector[A] = OneAnd(head, tail.toVector)

  def NonEmptyStream[A](head: A, tail: Stream[A] = Stream.empty): NonEmptyStream[A] = OneAnd(head, tail)
  def NonEmptyStream[A](head: A, tail: A*): NonEmptyStream[A] = OneAnd(head, tail.toStream)
}
