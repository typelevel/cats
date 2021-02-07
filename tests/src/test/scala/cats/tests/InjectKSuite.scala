package cats.tests

import cats.{:<:, Functor, InjectK}
import cats.data.EitherK
import cats.kernel.Eq
import cats.laws.discipline.InjectKTests
import org.scalacheck._
import cats.syntax.eq._
import org.scalacheck.Prop._

class InjectKSuite extends CatsSuite {

  sealed trait Test1Algebra[A]

  case class Test1[A](value: Int, f: Int => A) extends Test1Algebra[A]

  object Test1Algebra {
    implicit def test1AlgebraAFunctor: Functor[Test1Algebra] =
      new Functor[Test1Algebra] {
        def map[A, B](a: Test1Algebra[A])(f: A => B): Test1Algebra[B] =
          a match {
            case Test1(k, h) => Test1(k, x => f(h(x)))
          }
      }

    implicit def test1AlgebraArbitrary[A](implicit
      seqArb: Arbitrary[Int],
      intAArb: Arbitrary[Int => A]
    ): Arbitrary[Test1Algebra[A]] =
      Arbitrary(for { s <- seqArb.arbitrary; f <- intAArb.arbitrary } yield Test1(s, f))

    implicit def test1AlgebraEq[A](implicit ev: Eq[A]): Eq[Test1Algebra[A]] = Eq.fromUniversalEquals
  }

  sealed trait Test2Algebra[A]

  case class Test2[A](value: Int, f: Int => A) extends Test2Algebra[A]

  object Test2Algebra {
    implicit def test2AlgebraAFunctor: Functor[Test2Algebra] =
      new Functor[Test2Algebra] {
        def map[A, B](a: Test2Algebra[A])(f: A => B): Test2Algebra[B] =
          a match {
            case Test2(k, h) => Test2(k, x => f(h(x)))
          }
      }

    implicit def test2AlgebraArbitrary[A](implicit
      seqArb: Arbitrary[Int],
      intAArb: Arbitrary[Int => A]
    ): Arbitrary[Test2Algebra[A]] =
      Arbitrary(for { s <- seqArb.arbitrary; f <- intAArb.arbitrary } yield Test2(s, f))

    implicit def test2AlgebraEq[A](implicit ev: Eq[A]): Eq[Test2Algebra[A]] = Eq.fromUniversalEquals
  }

  type T[A] = EitherK[Test1Algebra, Test2Algebra, A]

  implicit def tArbitrary[A](implicit
    arb1: Arbitrary[Test1Algebra[A]],
    arb2: Arbitrary[Test2Algebra[A]]
  ): Arbitrary[T[A]] =
    Arbitrary(Gen.oneOf(arb1.arbitrary.map(EitherK.leftc(_): T[A]), arb2.arbitrary.map(EitherK.rightc(_): T[A])))

  test("inj & prj") {
    def distr[F[_], A](f1: F[A],
                       f2: F[A]
    )(implicit F: Functor[F], I0: Test1Algebra :<: F, I1: Test2Algebra :<: F): Option[Int] =
      for {
        Test1(x, _) <- I0.prj(f1)
        Test2(y, _) <- I1.prj(f2)
      } yield x + y

    forAll { (x: Int, y: Int) =>
      val expr1: T[Int] = InjectK[Test1Algebra, T].inj(Test1(x, _ + 1))
      val expr2: T[Int] = InjectK[Test2Algebra, T].inj(Test2(y, _ * 2))
      val res = distr[T, Int](expr1, expr2)
      assert(res === (Some(x + y)))
    }
  }

  test("apply & unapply") {
    def distr[F[_], A](f1: F[A],
                       f2: F[A]
    )(implicit F: Functor[F], I0: Test1Algebra :<: F, I1: Test2Algebra :<: F): Option[Int] =
      for {
        Test1(x, _) <- I0.unapply(f1)
        Test2(y, _) <- I1.unapply(f2)
      } yield x + y

    forAll { (x: Int, y: Int) =>
      val expr1: T[Int] = InjectK[Test1Algebra, T].apply(Test1(x, _ + 1))
      val expr2: T[Int] = InjectK[Test2Algebra, T].apply(Test2(y, _ * 2))
      val res = distr[T, Int](expr1, expr2)
      assert(res === (Some(x + y)))
    }
  }

  test("apply in left") {
    forAll { (y: Test1Algebra[Int]) =>
      assert(InjectK[Test1Algebra, T].inj(y) == EitherK(Left(y)) === true)
    }
  }

  test("apply in right") {
    forAll { (y: Test2Algebra[Int]) =>
      assert(InjectK[Test2Algebra, T].inj(y) == EitherK(Right(y)) === true)
    }
  }

  test("null identity") {
    val listIntNull = null.asInstanceOf[List[Int]]
    assert(InjectK.catsReflexiveInjectKInstance[List].inj[Int](listIntNull) === listIntNull)
    assert(InjectK.catsReflexiveInjectKInstance[List].prj[Int](listIntNull) === (Some(listIntNull)))
  }

  checkAll("InjectK[Test1Algebra, T]", InjectKTests[Test1Algebra, T].injectK[String])
  checkAll("InjectK[Test2Algebra, T]", InjectKTests[Test2Algebra, T].injectK[String])
}
