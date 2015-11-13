package cats
package free

import cats.arrow.NaturalTransformation
import cats.data.{Xor, Coproduct}
import cats.laws.discipline.arbitrary
import cats.tests.CatsSuite
import org.scalacheck._
import org.scalactic.CanEqual
import Free._

class InjectTests extends CatsSuite {

  import Inject._

  sealed trait Test1Algebra[A]

  case class Test1(keys: Seq[Int]) extends Test1Algebra[Seq[Int]]

  sealed trait Test2Algebra[A]

  case class Test2(keys: Seq[Int]) extends Test2Algebra[Seq[Int]]

  type T[A] = Coproduct[Test1Algebra, Test2Algebra, A]

  implicit def test1Arbitrary(implicit seqArb: Arbitrary[Seq[Int]]): Arbitrary[Test1] =
    Arbitrary(for {s <- seqArb.arbitrary} yield Test1(s))

  implicit def test2Arbitrary(implicit seqArb: Arbitrary[Seq[Int]]): Arbitrary[Test2] =
    Arbitrary(for {s <- seqArb.arbitrary} yield Test2(s))

  def or[F[_], G[_], H[_]](f: F ~> H, g: G ~> H): Coproduct[F, G, ?] ~> H =
    new (Coproduct[F, G, ?] ~> H) {
      def apply[A](fa: Coproduct[F, G, A]): H[A] = fa.run match {
        case Xor.Left(ff) => f(ff)
        case Xor.Right(gg) => g(gg)
      }
    }

  object Test1Interpreter extends (Test1Algebra ~> Id) {
    override def apply[A](fa: Test1Algebra[A]): Id[A] = fa match {
      case Test1(k) => k
    }
  }

  object Test2Interpreter extends (Test2Algebra ~> Id) {
    override def apply[A](fa: Test2Algebra[A]): Id[A] = fa match {
      case Test2(k) => k
    }
  }

  val coProductInterpreter: T ~> Id = or(Test1Interpreter, Test2Interpreter)

  def lift[F[_], G[_], A](fa: F[A])(implicit I: Inject[F, G]): Free[G, A] =
    Free.liftF(I.inj(fa))

  class Ops[F[_]](implicit I1: Inject[Test1Algebra, F], I2: Inject[Test2Algebra, F]) {

    def test1(seq: Seq[Int]): Free[T, Seq[Int]] = lift[Test1Algebra, T, Seq[Int]](Test1(seq))

    def test2(seq: Seq[Int]): Free[T, Seq[Int]] = lift[Test2Algebra, T, Seq[Int]](Test2(seq))

  }

  object Ops {

    implicit def ops[F[_]](implicit I1: Inject[Test1Algebra, F], I2: Inject[Test2Algebra, F]): Ops[F] = new Ops[F]

  }

  val ops: Ops[T] = implicitly[Ops[T]]

  test("inj") {
    forAll { (seq1: Seq[Int], seq2: Seq[Int]) =>
      val res =
        for {
          a <- ops.test1(seq1)
          b <- ops.test2(seq2)
        } yield a ++ b
      ((res foldMap coProductInterpreter) == Id.pure(seq1 ++ seq2)) should ===(true)
    }
  }

  test("apply in left") {
    forAll { (y: Test1) =>
      Inject[Test1Algebra, T].inj(y) == Coproduct(Xor.Left(y)) should ===(true)
    }
  }

  test("apply in right") {
    forAll { (y: Test2) =>
      Inject[Test2Algebra, T].inj(y) == Coproduct(Xor.Right(y)) should ===(true)
    }
  }

}
