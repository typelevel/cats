package cats
package free

import cats.tests.CatsSuite
import cats.arrow.FunctionK
import cats.data.Xor
import cats.laws.discipline.{CartesianTests, MonadRecTests, SerializableTests}
import cats.laws.discipline.arbitrary.function0Arbitrary

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbFunction1

class FreeTests extends CatsSuite {
  import FreeTests._

  implicit val iso = CartesianTests.Isomorphisms.invariant[Free[Option, ?]]

  checkAll("Free[Option, ?]", MonadRecTests[Free[Option, ?]].monadRec[Int, Int, Int])
  checkAll("MonadRec[Free[Option, ?]]", SerializableTests.serializable(MonadRec[Free[Option, ?]]))

  test("mapSuspension id"){
    forAll { x: Free[List, Int] =>
      x.mapSuspension(FunctionK.id[List]) should === (x)
    }
  }

  test("suspend doesn't change value"){
    forAll { x: Free[List, Int] =>
      Free.suspend(x) should === (x)
    }
  }

  test("suspend is lazy"){
    def yikes[F[_], A]: Free[F, A] = throw new RuntimeException("blargh")
    // this shouldn't throw an exception unless we try to run it
    val _ = Free.suspend(yikes[Option, Int])
  }

  test("mapSuspension consistent with foldMap"){
    forAll { x: Free[List, Int] =>
      val mapped = x.mapSuspension(headOptionU)
      val folded = mapped.foldMap(FunctionK.id[Option])
      folded should === (x.foldMap(headOptionU))
    }
  }

  test("tailRecM is stack safe") {
    val n = 50000
    val fa = MonadRec[Free[Option, ?]].tailRecM(0)(i =>
      Free.pure[Option, Int Xor Int](if(i < n) Xor.Left(i+1) else Xor.Right(i)))
    fa should === (Free.pure[Option, Int](n))
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

    def runner: FunctionK[FTestApi,Id] = new FunctionK[FTestApi,Id] {
      def apply[A](fa: FTestApi[A]): Id[A] = fa match {
        case TB(i) => i+1
      }
    }

    assert(10000 == a(0).foldMap(runner))
  }
}

object FreeTests extends FreeTestsInstances {
  import cats.std.function._

  implicit def trampolineArbitrary[A:Arbitrary]: Arbitrary[Trampoline[A]] =
    freeArbitrary[Function0, A]

  implicit def trampolineEq[A:Eq]: Eq[Trampoline[A]] =
    freeEq[Function0, A]
}

sealed trait FreeTestsInstances {
  val headOptionU: FunctionK[List,Option] = new FunctionK[List,Option] {
    def apply[A](fa: List[A]): Option[A] = fa.headOption
  }

  private def freeGen[F[_], A](maxDepth: Int)(implicit F: Arbitrary[F[A]], A: Arbitrary[A]): Gen[Free[F, A]] = {
    val noGosub = Gen.oneOf(
      A.arbitrary.map(Free.pure[F, A]),
      F.arbitrary.map(Free.liftF[F, A]))

    val nextDepth = Gen.chooseNum(1, maxDepth - 1)

    def withGosub = for {
      fDepth <- nextDepth
      freeDepth <- nextDepth
      f <- arbFunction1[A, Free[F, A]](Arbitrary(freeGen[F, A](fDepth))).arbitrary
      freeFA <- freeGen[F, A](freeDepth)
    } yield freeFA.flatMap(f)

    if (maxDepth <= 1) noGosub
    else Gen.oneOf(noGosub, withGosub)
  }

  implicit def freeArbitrary[F[_], A](implicit F: Arbitrary[F[A]], A: Arbitrary[A]): Arbitrary[Free[F, A]] =
    Arbitrary(freeGen[F, A](4))

  implicit def freeEq[S[_]: Monad, A](implicit SA: Eq[S[A]]): Eq[Free[S, A]] =
    new Eq[Free[S, A]] {
      def eqv(a: Free[S, A], b: Free[S, A]): Boolean =
        SA.eqv(a.runM(identity),  b.runM(identity))
    }
}
