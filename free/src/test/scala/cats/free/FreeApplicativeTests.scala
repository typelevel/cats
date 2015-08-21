package cats
package free

import cats.arrow.NaturalTransformation
import cats.laws.discipline.{ArbitraryK, ApplicativeTests, SerializableTests}
import cats.tests.CatsSuite

import org.scalacheck.{Arbitrary, Gen}

class FreeApplicativeTests extends CatsSuite {
  implicit def freeApplicativeArbitrary[F[_], A](implicit F: ArbitraryK[F], A: Arbitrary[A]): Arbitrary[FreeApplicative[F, A]] =
    Arbitrary(
      Gen.oneOf(
        A.arbitrary.map(FreeApplicative.pure[F, A]),
        F.synthesize[A].arbitrary.map(FreeApplicative.lift[F, A])))

  implicit def freeApplicativeArbitraryK[F[_]](implicit F: ArbitraryK[F]): ArbitraryK[FreeApplicative[F, ?]] =
    new ArbitraryK[FreeApplicative[F, ?]]{
      def synthesize[A: Arbitrary]: Arbitrary[FreeApplicative[F, A]] =
        freeApplicativeArbitrary[F, A]
    }

  implicit def freeApplicativeEq[S[_]: Applicative, A](implicit SA: Eq[S[A]]): Eq[FreeApplicative[S, A]] =
    new Eq[FreeApplicative[S, A]] {
      def eqv(a: FreeApplicative[S, A], b: FreeApplicative[S, A]): Boolean = {
        val nt = NaturalTransformation.id[S]
        SA.eqv(a.foldMap(nt), b.foldMap(nt))
      }
    }

  checkAll("FreeApplicative[Option, ?]", ApplicativeTests[FreeApplicative[Option, ?]].applicative[Int, Int, Int])
  checkAll("Monad[FreeApplicative[Option, ?]]", SerializableTests.serializable(Applicative[FreeApplicative[Option, ?]]))

  test("FreeApplicative#fold") {
    val n = 2
    val o1 = Option(1)
    val o2 = Applicative[Option].pure(n)

    val x = FreeApplicative.lift[Option, Int](o1)
    val y = FreeApplicative.pure[Option, Int](n)
    val f = x.map(i => (j: Int) => i + j)
    val r = y.ap(f)
    assert(r.fold == Apply[Option].map2(o1, o2)(_ + _))
  }

  test("FreeApplicative#compile") {
    val x = FreeApplicative.lift[Id, Int](1)
    val y = FreeApplicative.pure[Id, Int](2)
    val f = x.map(i => (j: Int) => i + j)
    val nt = NaturalTransformation.id[Id]
    val r1 = y.ap(f)
    val r2 = r1.compile(nt)
    assert(r1.foldMap(nt) == r2.foldMap(nt))
  }

  test("FreeApplicative#monad") {
    val x = FreeApplicative.lift[Id, Int](1)
    val y = FreeApplicative.pure[Id, Int](2)
    val f = x.map(i => (j: Int) => i + j)
    val r1 = y.ap(f)
    val r2 = r1.monad
    val nt =
      new NaturalTransformation[Id, Id] {
        def apply[A](fa: Id[A]): Id[A] = fa
      }
    assert(r1.foldMap(nt) == r2.foldMap(nt))
  }
}
