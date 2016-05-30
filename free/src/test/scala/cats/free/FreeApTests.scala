package cats
package free

import cats.arrow.NaturalTransformation
import cats.laws.discipline.{CartesianTests, MonadTests, SerializableTests}
import cats.syntax.cartesian._
import cats.tests.CatsSuite

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.{arbFunction1, arbFunction2}

class FreeApTests extends CatsSuite {

  import FreeApTests._

  implicit val iso = CartesianTests.Isomorphisms.invariant[FreeAp[Option, ?]]

  checkAll("FreeAp[Option, ?]", MonadTests[FreeAp[Option, ?]].monad[Int, Int, Int])
  checkAll("Monad[FreeAp[Option, ?]]", SerializableTests.serializable(Monad[FreeAp[Option, ?]]))

  test("FreeAp's suspend is lazy"){
    def yikes[F[_], A]: FreeAp[F, A] = throw new RuntimeException("blargh")
    // this shouldn't throw an exception unless we try to run it
    val _ = FreeAp.suspend(yikes[Option, Int])
  }

  test("foldMap is stack safe when the targeted monad is stack safe") {
    trait FTestApi[A]
    case class TB(i: Int) extends FTestApi[Int]

    type FTest[A] = FreeAp[FTestApi, A]

    def tb(i: Int): FTest[Int] = FreeAp.liftF(TB(i))

    def a(i: Int): FTest[Int] = for {
      j <- tb(i)
      z <- if (j<10000) a(j) else FreeAp.pure[FTestApi, Int](j)
    } yield z

    def runner: NaturalTransformation[FTestApi, Eval] = new NaturalTransformation[FTestApi, Eval] {
      def apply[A](fa: FTestApi[A]): Eval[A] = fa match {
        case TB(i) => Eval.now(i+1)
      }
    }

    assert(10000 == a(0).foldMap(runner).value)
  }
}

object FreeApTests {

  private def freeApGen[F[_], A](maxDepth: Int)(implicit F: Arbitrary[F[A]], A: Arbitrary[A]): Gen[FreeAp[F, A]] = {
    val noGosub = Gen.oneOf(
      A.arbitrary.map(FreeAp.pure[F, A]),
      F.arbitrary.map(FreeAp.liftF[F, A]))

    val nextDepth = Gen.chooseNum(1, maxDepth - 1)

    def withGosub = for {
      fDepth <- nextDepth
      freeDepth <- nextDepth
      f <- arbFunction1[A, FreeAp[F, A]](Arbitrary(freeApGen[F, A](fDepth))).arbitrary
      freeFA <- freeApGen[F, A](freeDepth)
    } yield freeFA.flatMap(f)

    def withAp = for {
      fDepth <- nextDepth
      freeDepth <- nextDepth
      fA <- freeApGen[F, A](fDepth)
      freeFA <- freeApGen[F, A](freeDepth)
      f <- arbFunction2[A, A, A].arbitrary
    } yield (fA |@| freeFA).map(f)

    if (maxDepth <= 1) {
      noGosub
    }
    else {
      Gen.oneOf(noGosub, withGosub, withAp)
    }
  }

  implicit def freeArbitrary[F[_], A](implicit F: Arbitrary[F[A]], A: Arbitrary[A]): Arbitrary[FreeAp[F, A]] =
    Arbitrary(freeApGen[F, A](3))

  def ide[S[_]] = new NaturalTransformation[Lambda[a => S[FreeAp[S, a]]], Lambda[a => S[FreeAp[S, a]]]] {
    def apply[A](id: S[FreeAp[S, A]]) = id

  }

  implicit def freeApEq[S[_]: Monad, A](implicit SA: Eq[S[A]]): Eq[FreeAp[S, A]] =
    new Eq[FreeAp[S, A]] {
      def eqv(a: FreeAp[S, A], b: FreeAp[S, A]): Boolean =
        SA.eqv(a.runM(ide),  b.runM(ide))
    }
}
