package cats
package free

import cats.arrow.FunctionK
import cats.data.Xor
import cats.tests.CatsSuite
import cats.data.Coproduct
import org.scalacheck._

class InjectTests extends CatsSuite {

  import Inject._

  sealed trait Test1Algebra[A]

  case class Test1[A](value : Int, f: Int => A) extends Test1Algebra[A]

  sealed trait Test2Algebra[A]

  case class Test2[A](value : Int, f: Int => A) extends Test2Algebra[A]

  type T[A] = Coproduct[Test1Algebra, Test2Algebra, A]

  implicit def test1AlgebraAFunctor: Functor[Test1Algebra] =
    new Functor[Test1Algebra] {
      def map[A, B](a: Test1Algebra[A])(f: A => B): Test1Algebra[B] = a match {
        case Test1(k, h) => Test1(k, x => f(h(x)))
      }
    }

  implicit def test2AlgebraAFunctor: Functor[Test2Algebra] =
    new Functor[Test2Algebra] {
      def map[A, B](a: Test2Algebra[A])(f: A => B): Test2Algebra[B] = a match {
        case Test2(k, h) => Test2(k, x => f(h(x)))
      }
    }

  implicit def test1Arbitrary[A](implicit seqArb: Arbitrary[Int], intAArb : Arbitrary[Int => A]): Arbitrary[Test1[A]] =
    Arbitrary(for {s <- seqArb.arbitrary; f <- intAArb.arbitrary} yield Test1(s, f))

  implicit def test2Arbitrary[A](implicit seqArb: Arbitrary[Int], intAArb : Arbitrary[Int => A]): Arbitrary[Test2[A]] =
    Arbitrary(for {s <- seqArb.arbitrary; f <- intAArb.arbitrary} yield Test2(s, f))

  object Test1Interpreter extends FunctionK[Test1Algebra,Id] {
    override def apply[A](fa: Test1Algebra[A]): Id[A] = fa match {
      case Test1(k, h) => h(k)
    }
  }

  object Test2Interpreter extends FunctionK[Test2Algebra,Id] {
    override def apply[A](fa: Test2Algebra[A]): Id[A] = fa match {
      case Test2(k, h) => h(k)
    }
  }

  val coProductInterpreter: FunctionK[T,Id] = Test1Interpreter or Test2Interpreter

  val x: Free[T, Int] = Free.inject[Test1Algebra, T](Test1(1, identity))

  test("inj") {
    forAll { (x: Int, y: Int) =>
        def res[F[_]]
            (implicit I0: Test1Algebra :<: F,
            I1: Test2Algebra :<: F): Free[F, Int] = {
          for {
            a <- Free.inject[Test1Algebra, F](Test1(x, identity))
            b <- Free.inject[Test2Algebra, F](Test2(y, identity))
          } yield a + b
        }
      (res[T] foldMap coProductInterpreter) == (x + y) should ===(true)
    }
  }

  test("prj") {
    def distr[F[_], A](f: Free[F, A])
                      (implicit
                       F: Functor[F],
                       I0: Test1Algebra :<: F,
                       I1: Test2Algebra :<: F): Option[Free[F, A]] =
      for {
        Test1(x, h) <- match_[F, Test1Algebra, A](f)
        Test2(y, k) <- match_[F, Test2Algebra, A](h(x))
      } yield k(x + y)

    forAll { (x: Int, y: Int) =>
      val expr1: Free[T, Int] = Inject.inject[T, Test1Algebra, Int](Test1(x, Free.pure))
      val expr2: Free[T, Int] = Inject.inject[T, Test2Algebra, Int](Test2(y, Free.pure))
      val res = distr[T, Int](expr1 >> expr2)
      res == Some(Free.pure(x + y)) should ===(true)
    }
  }

  test("apply in left") {
    forAll { (y: Test1[Int]) =>
      Inject[Test1Algebra, T].inj(y) == Coproduct(Xor.Left(y)) should ===(true)
    }
  }

  test("apply in right") {
    forAll { (y: Test2[Int]) =>
      Inject[Test2Algebra, T].inj(y) == Coproduct(Xor.Right(y)) should ===(true)
    }
  }

}
