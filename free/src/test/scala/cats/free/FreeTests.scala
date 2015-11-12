package cats
package free

import cats.tests.CatsSuite
import cats.laws.discipline.{MonadTests, SerializableTests}
import org.scalacheck.{Arbitrary, Gen}

class FreeTests extends CatsSuite {

  implicit def freeArbitrary[F[_], A](implicit F: Arbitrary[F[A]], A: Arbitrary[A]): Arbitrary[Free[F, A]] =
    Arbitrary(
      Gen.oneOf(
        A.arbitrary.map(Free.pure[F, A]),
        F.arbitrary.map(Free.liftF[F, A])))

  implicit def freeEq[S[_]: Monad, A](implicit SA: Eq[S[A]]): Eq[Free[S, A]] =
    new Eq[Free[S, A]] {
      def eqv(a: Free[S, A], b: Free[S, A]): Boolean =
        SA.eqv(a.runM(identity),  b.runM(identity))
    }

  checkAll("Free[Option, ?]", MonadTests[Free[Option, ?]].monad[Int, Int, Int])
  checkAll("Monad[Free[Option, ?]]", SerializableTests.serializable(Monad[Free[Option, ?]]))
}
