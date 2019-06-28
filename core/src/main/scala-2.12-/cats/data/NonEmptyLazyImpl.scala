package cats.data

object NonEmptyLazyImpl {
  type Type[+A] = NonEmptyStream[A]
}
