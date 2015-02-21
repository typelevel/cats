package cats.laws
package discipline

import org.scalacheck.Arbitrary

trait ArbitraryK2[F[_, _]] {
  def synthesize[A: Arbitrary, B: Arbitrary]: Arbitrary[F[A, B]]
}

object ArbitraryK2 {
  implicit val function1: ArbitraryK2[Function1] =
    new ArbitraryK2[Function1] {
      def synthesize[A: Arbitrary, B: Arbitrary]: Arbitrary[A => B] = implicitly
    }
}
