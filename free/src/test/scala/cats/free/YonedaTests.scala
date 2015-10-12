package cats
package free

import cats.tests.CatsSuite
import cats.laws.discipline.{ArbitraryK, FunctorTests, SerializableTests}

import org.scalacheck.Arbitrary

class YonedaTests extends CatsSuite {
  implicit def yonedaArbitraryK[F[_] : Functor](implicit F: ArbitraryK[F]): ArbitraryK[Yoneda[F, ?]] =
    new ArbitraryK[Yoneda[F, ?]]{
      def synthesize[A: Arbitrary]: Arbitrary[Yoneda[F, A]] =
        Arbitrary(F.synthesize[A].arbitrary.map(Yoneda(_)))
    }

  implicit def yonedaArbitrary[F[_] : Functor : ArbitraryK, A : Arbitrary]: Arbitrary[Yoneda[F, A]] = yonedaArbitraryK[F].synthesize[A]

  implicit def yonedaEq[F[_]: Functor, A](implicit FA: Eq[F[A]]): Eq[Yoneda[F, A]] =
    new Eq[Yoneda[F, A]] {
      def eqv(a: Yoneda[F, A], b: Yoneda[F, A]): Boolean = FA.eqv(a.run, b.run)
    }

  checkAll("Yoneda[Option, ?]", FunctorTests[Yoneda[Option, ?]].functor[Int, Int, Int])
  checkAll("Functor[Yoneda[Option, ?]]", SerializableTests.serializable(Functor[Yoneda[Option, ?]]))

  test("toCoyoneda and then toYoneda is identity"){
    forAll{ (y: Yoneda[Option, Int]) =>
      y.toCoyoneda.toYoneda should === (y)
    }
  }
}
