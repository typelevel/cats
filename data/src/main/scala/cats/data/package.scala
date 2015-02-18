package cats

import cats.data.IList.INil

package object data {
  type NonEmptyList[A] = OneAnd[A, IList]

  def NonEmptyList[A](head: A, tail: IList[A] = INil()) = OneAnd[A, IList](head, tail)
}
