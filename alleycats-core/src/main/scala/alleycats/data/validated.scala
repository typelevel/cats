package alleycats.data

import cats.data.Validated
import cats.data.Validated.Invalid

object validated {

  /*
   * The contents of FlatMap is inspired by and derived from
   * object FlatMap and related contents in Scalaz's Validation
   * originally contributed by Stephen Compall (@S11001001).
   */
  object FlatMap {

    final class ValidatedFlatMap[E, A](val self: Validated[E, A]) extends AnyVal {
      def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = self.fold(Invalid(_), a => f(a))
    }

    @inline implicit def ValidationFlatMapRequested[E, A](d: Validated[E, A]): ValidatedFlatMap[E, A] =
      new ValidatedFlatMap(d)
  }

}
