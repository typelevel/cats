package cats

/**
 * AlgebraK abstracts over type classes representing algebras at
 * the kind level.
 *
 * Type classes extending AlgebraK, such as SemigroupK and
 * MonoidKi, are useful when their type parameter F[_] has a
 * structure that can be combined for any particular type.
 */
trait AlgebraK[+A[_], F[_]] {
  def apply[T]: A[F[T]]
}

