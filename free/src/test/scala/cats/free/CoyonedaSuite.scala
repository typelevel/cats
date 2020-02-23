package cats
package free

import cats.tests.CatsSuite
import cats.arrow.FunctionK
import cats.laws.discipline.{FunctorTests, SerializableTests}

import org.scalacheck.Arbitrary

class CoyonedaSuite extends CatsSuite {
  implicit def coyonedaArbitrary[F[_]: Functor, A: Arbitrary](implicit F: Arbitrary[F[A]]): Arbitrary[Coyoneda[F, A]] =
    Arbitrary(F.arbitrary.map(Coyoneda.lift))

  implicit def coyonedaEq[F[_]: Functor, A](implicit FA: Eq[F[A]]): Eq[Coyoneda[F, A]] =
    new Eq[Coyoneda[F, A]] {
      def eqv(a: Coyoneda[F, A], b: Coyoneda[F, A]): Boolean = FA.eqv(a.run, b.run)
    }

  checkAll("Coyoneda[Option, *]", FunctorTests[Coyoneda[Option, *]].functor[Int, Int, Int])
  checkAll("Functor[Coyoneda[Option, *]]", SerializableTests.serializable(Functor[Coyoneda[Option, *]]))

  test("toYoneda and then toCoyoneda is identity") {
    forAll((y: Coyoneda[Option, Int]) => y.toYoneda.toCoyoneda should ===(y))
  }

  test("mapK and run is same as applying natural trans") {
    val nt = λ[FunctionK[Option, List]](_.toList)
    val o = Option("hello")
    val c = Coyoneda.lift(o)
    c.mapK(nt).run should ===(nt(o))
  }

  test("map order") {
    Coyoneda
      .lift[Option, Int](Some(0))
      .map(_ + 1)
      .map(_ * 3)
      .run === Some(3)
  }

  test("stack-safe map") {
    def loop(n: Int, acc: Coyoneda[Option, Int]): Coyoneda[Option, Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.map(_ + 1))

    loop(20000, Coyoneda.lift[Option, Int](Some(1))).run
  }
}
