package cats

package object data {
  type NonEmptyList[A] = OneAnd[A, List]

  def NonEmptyList[A](head: A, tail: List[A] = Nil) = OneAnd[A, List](head, tail)
}
