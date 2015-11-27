package cats
package free

import cats.arrow.NaturalTransformation
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

  test("mapSuspension id"){
    forAll { x: Free[List, Int] =>
      x.mapSuspension(NaturalTransformation.id[List]) should === (x)
    }
  }

  test("foldMap is stack safe") {
    trait FTestApi[A]
    case class TB(i: Int) extends FTestApi[Int]

    type FTest[A] = Free[FTestApi, A]

    def tb(i: Int): FTest[Int] = Free.liftF(TB(i))

    def a(i: Int): FTest[Int] = for {
      j <- tb(i)
      z <- if (j<10000) a(j) else Free.pure[FTestApi, Int](j)
    } yield z

    def runner: FTestApi ~> Id = new (FTestApi ~> Id) {
      def apply[A](fa: FTestApi[A]): Id[A] = fa match {
        case TB(i) => i+1
      }
    }

    assert(10000 == a(0).foldMap(runner))
  }
}
