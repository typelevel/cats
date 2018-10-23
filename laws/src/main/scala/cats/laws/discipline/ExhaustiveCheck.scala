package cats
package laws
package discipline

/**
 * An `ExhuastiveCheck[A]` instance can be used similarly to a Scalacheck
 * `Gen[A]` instance, but differs in that it generates a `Stream` of the entire
 * domain of values as opposed to generating a random sampling of values.
 */
trait ExhaustiveCheck[A] extends Serializable { self =>
  def allValues: Stream[A]
}

object ExhaustiveCheck {
  def apply[A](implicit A: ExhaustiveCheck[A]): ExhaustiveCheck[A] = A

  def instance[A](values: Stream[A]): ExhaustiveCheck[A] = new ExhaustiveCheck[A] {
    val allValues: Stream[A] = values
  }

  implicit val catsLawsExhaustiveCheckForBoolean: ExhaustiveCheck[Boolean] =
    instance(Stream(false, true))

  /**
   * Warning: the domain of (A, B) is the cross-product of the domain of `A` and the domain of `B`.
   */
  implicit def catsLawsExhaustiveCheckForTuple2[A, B](implicit A: ExhaustiveCheck[A],
                                                      B: ExhaustiveCheck[B]): ExhaustiveCheck[(A, B)] =
    instance(A.allValues.flatMap(a => B.allValues.map(b => (a, b))))

  /**
   * Warning: the domain of (A, B, C) is the cross-product of the 3 domains.
   */
  implicit def catsLawsExhaustiveCheckForTuple3[A, B, C](implicit A: ExhaustiveCheck[A],
                                                         B: ExhaustiveCheck[B],
                                                         C: ExhaustiveCheck[C]): ExhaustiveCheck[(A, B, C)] =
    instance(
      for {
        a <- A.allValues
        b <- B.allValues
        c <- C.allValues
      } yield (a, b, c)
    )

  implicit def catsLawsExhaustiveCheckForEither[A, B](implicit A: ExhaustiveCheck[A],
                                                      B: ExhaustiveCheck[B]): ExhaustiveCheck[Either[A, B]] =
    instance(A.allValues.map(Left(_)) ++ B.allValues.map(Right(_)))

  implicit def catsLawsExhaustiveCheckForOption[A](implicit A: ExhaustiveCheck[A]): ExhaustiveCheck[Option[A]] =
    instance(Stream.cons(None, A.allValues.map(Some(_))))
}
