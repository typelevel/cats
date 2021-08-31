package cats.free

import cats.Functor
import cats.instances.all._
import cats.kernel.Eq
import cats.laws.discipline.{FunctorTests, SerializableTests}
import cats.tests.CatsSuite
import org.scalacheck.Arbitrary
import cats.syntax.eq._
import org.scalacheck.Prop._

class YonedaSuite extends CatsSuite {
  implicit def yonedaArbitrary[F[_]: Functor, A](implicit F: Arbitrary[F[A]]): Arbitrary[Yoneda[F, A]] =
    Arbitrary(F.arbitrary.map(Yoneda(_)))

  implicit def yonedaEq[F[_]: Functor, A](implicit FA: Eq[F[A]]): Eq[Yoneda[F, A]] =
    new Eq[Yoneda[F, A]] {
      def eqv(a: Yoneda[F, A], b: Yoneda[F, A]): Boolean = FA.eqv(a.run, b.run)
    }

  checkAll("Yoneda[Option, *]", FunctorTests[Yoneda[Option, *]].functor[Int, Int, Int])
  checkAll("Functor[Yoneda[Option, *]]", SerializableTests.serializable(Functor[Yoneda[Option, *]]))

  property("toCoyoneda and then toYoneda is identity") {
    forAll { (y: Yoneda[Option, Int]) =>
      assert(y.toCoyoneda.toYoneda === y)
    }
  }
}
