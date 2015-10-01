package cats
package free

import cats.arrow.NaturalTransformation
import cats.tests.CatsSuite
import cats.laws.discipline.{ArbitraryK, FunctorTests, SerializableTests}

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

class CoyonedaTests extends CatsSuite {
  implicit def coyonedaArbitraryK[F[_] : Functor](implicit F: ArbitraryK[F]): ArbitraryK[Coyoneda[F, ?]] =
    new ArbitraryK[Coyoneda[F, ?]]{
      def synthesize[A: Arbitrary]: Arbitrary[Coyoneda[F, A]] = coyonedaArbitrary[F, A]
    }

  implicit def coyonedaArbitrary[F[_] : Functor, A : Arbitrary](implicit F: ArbitraryK[F]): Arbitrary[Coyoneda[F, A]] =
    Arbitrary(F.synthesize[A].arbitrary.map(Coyoneda.lift))

  implicit def coyonedaEq[F[_]: Functor, A](implicit FA: Eq[F[A]]): Eq[Coyoneda[F, A]] =
    new Eq[Coyoneda[F, A]] {
      def eqv(a: Coyoneda[F, A], b: Coyoneda[F, A]): Boolean = FA.eqv(a.run, b.run)
    }

  checkAll("Coyoneda[Option, ?]", FunctorTests[Coyoneda[Option, ?]].functor[Int, Int, Int])
  checkAll("Functor[Coyoneda[Option, ?]]", SerializableTests.serializable(Functor[Coyoneda[Option, ?]]))

  test("toYoneda and then toCoyoneda is identity"){
    forAll{ (y: Coyoneda[Option, Int]) =>
      y.toYoneda.toCoyoneda should === (y)
    }
  }

  test("transform and run is same as applying natural trans") {
      val nt =
        new NaturalTransformation[Option, List] {
          def apply[A](fa: Option[A]): List[A] = fa.toList
        }
      val o = Option("hello")
      val c = Coyoneda.lift(o)
      c.transform(nt).run should === (nt(o))
  }
}
