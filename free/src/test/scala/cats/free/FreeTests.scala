package cats
package free

import cats.tests.CatsSuite
import cats.laws.discipline.{ArbitraryK, EqK, MonadTests, SerializableTests}
import org.scalacheck.{Arbitrary, Gen}

class FreeTests extends CatsSuite {

  implicit def freeArbitrary[F[_], A](implicit F: ArbitraryK[F], A: Arbitrary[A]): Arbitrary[Free[F, A]] =
    Arbitrary(
      Gen.oneOf(
        A.arbitrary.map(Free.pure[F, A]),
        F.synthesize[A].arbitrary.map(Free.liftF[F, A])))

  implicit def freeArbitraryK[F[_]](implicit F: ArbitraryK[F]): ArbitraryK[Free[F, ?]] =
    new ArbitraryK[Free[F, ?]]{
      def synthesize[A: Arbitrary]: Arbitrary[Free[F, A]] =
        freeArbitrary[F, A]
    }

  implicit def freeEq[S[_]: Monad, A](implicit SA: Eq[S[A]]): Eq[Free[S, A]] =
    new Eq[Free[S, A]] {
      def eqv(a: Free[S, A], b: Free[S, A]): Boolean =
        SA.eqv(a.runM(identity),  b.runM(identity))
    }

  implicit def freeEqK[S[_]: EqK: Monad]: EqK[Free[S, ?]] =
    new EqK[Free[S, ?]] {
      def synthesize[A: Eq]: Eq[Free[S, A]] = {
        implicit val sa: Eq[S[A]] = EqK[S].synthesize[A]
        freeEq[S, A]
      }
    }

  checkAll("Free[Option, ?]", MonadTests[Free[Option, ?]].monad[Int, Int, Int])
  checkAll("Monad[Free[Option, ?]]", SerializableTests.serializable(Monad[Free[Option, ?]]))
}
