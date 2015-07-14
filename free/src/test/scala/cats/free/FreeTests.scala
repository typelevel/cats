package cats
package free

import cats.tests.CatsSuite
import cats.laws.discipline.{ArbitraryK, MonadTests, SerializableTests}
import org.scalacheck.{Arbitrary, Gen}

class FreeTests extends CatsSuite {

  implicit def freeArbitrary[F[_], A](implicit F: ArbitraryK[F], A: Arbitrary[A]): Arbitrary[Free[F, A]] =
    Arbitrary(
      Gen.oneOf(
        A.arbitrary.map(Free.Pure[F, A]),
        F.synthesize(freeArbitrary[F, A]).arbitrary.map(Free.Suspend[F, A])))

  implicit def freeArbitraryK[F[_]](implicit F: ArbitraryK[F]): ArbitraryK[Free[F, ?]] =
    new ArbitraryK[Free[F, ?]]{
      def synthesize[A: Arbitrary]: Arbitrary[Free[F, A]] =
        freeArbitrary[F, A]
    }

  implicit def freeEq[S[_]:Monad, A](implicit SA: Eq[S[A]]): Eq[Free[S, A]] =
    new Eq[Free[S, A]] {
      def eqv(a: Free[S, A], b: Free[S, A]): Boolean =
        SA.eqv(a.runM(identity),  b.runM(identity))
    }

  checkAll("Free[Option, ?]", MonadTests[Free[Option, ?]].monad[Int, Int, Int])
  checkAll("Monad[Free[Option, ?]]", SerializableTests.serializable(Monad[Free[Option, ?]]))

  // Check that expected implicits resolve.
  // As long as this code compiles, the "tests" pass.
  object ImplicitResolution {

    // before the addition of the freeCMonad helper, the monad instances for
    // FreeC were not found.
    sealed abstract class Foo[A]
    Monad[FreeC[Foo, ?]]
  }
}
