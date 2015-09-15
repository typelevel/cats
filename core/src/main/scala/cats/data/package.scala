package cats

package object data {
  type NonEmptyList[A] = OneAnd[A, List]
  type NonEmptyVector[A] = OneAnd[A, Vector]
  type NonEmptyStream[A] = OneAnd[A, Stream]
  type ValidatedNel[E, A] = Validated[NonEmptyList[E], A]

  def NonEmptyList[A](head: A, tail: List[A] = Nil): NonEmptyList[A] =
    OneAnd(head, tail)
  def NonEmptyList[A](head: A, tail: A*): NonEmptyList[A] =
    OneAnd[A, List](head, tail.toList)

  def NonEmptyVector[A](head: A, tail: Vector[A] = Vector.empty): NonEmptyVector[A] =
    OneAnd(head, tail)
  def NonEmptyVector[A](head: A, tail: A*): NonEmptyVector[A] =
    OneAnd(head, tail.toVector)

  def NonEmptyStream[A](head: A, tail: Stream[A] = Stream.empty): NonEmptyStream[A] =
    OneAnd(head, tail)
  def NonEmptyStream[A](head: A, tail: A*): NonEmptyStream[A] =
    OneAnd(head, tail.toStream)

  object NonEmptyList {
    def fromReducible[F[_], A](fa: F[A])(implicit F: Reducible[F]): Eval[NonEmptyList[A]] =
      F.reduceRightTo(fa)(a => NonEmptyList(a, Nil)) { (a, lnel) =>
        lnel.map { case OneAnd(h, t) => OneAnd(a, h :: t) }
      }

    def fromList[A](la: List[A]): Option[NonEmptyList[A]] =
      la match {
        case (h :: t) => Some(OneAnd(h, t))
        case Nil => None
      }
  }

  type ReaderT[F[_], A, B] = Kleisli[F, A, B]
  val ReaderT = Kleisli

  type Reader[A, B] = ReaderT[Id, A, B]
  object Reader {
    def apply[A, B](f: A => B): Reader[A, B] = ReaderT.function[Id, A, B](f)
  }
}
