package cats
package tests

import cats.data.{ Func, AppFunc }
import Func.appFunc
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class FuncTests extends CatsSuite {
  import cats.laws.discipline.eq._
  implicit def funcEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Func[F, A, B]] =
    Eq.by[Func[F, A, B], A => F[B]](_.run)
  implicit def appFuncEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[AppFunc[F, A, B]] =
    Eq.by[AppFunc[F, A, B], A => F[B]](_.run)

  implicit val iso = CartesianTests.Isomorphisms.invariant[Func[Option, Int, ?]]

  checkAll("Func[Option, Int, Int]", CartesianTests[Func[Option, Int, ?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Func[Option, Int, ?]]", SerializableTests.serializable(Cartesian[Func[Option, Int, ?]]))

  {
    implicit val catsDataApplicativeForFunc = Func.catsDataApplicativeForFunc[Option, Int]
    checkAll("Func[Option, Int, Int]", ApplicativeTests[Func[Option, Int, ?]].applicative[Int, Int, Int])
    checkAll("Applicative[Func[Option, Int, ?]]", SerializableTests.serializable(Applicative[Func[Option, Int, ?]]))
  }

  {
    implicit val catsDataApplyForFunc = Func.catsDataApplyForFunc[Option, Int]
    checkAll("Func[Option, Int, Int]", ApplyTests[Func[Option, Int, ?]].apply[Int, Int, Int])
    checkAll("Apply[Func[Option, Int, ?]]", SerializableTests.serializable(Apply[Func[Option, Int, ?]]))
  }

  {
    implicit val catsDataFunctorForFunc = Func.catsDataFunctorForFunc[Option, Int]
    checkAll("Func[Option, Int, Int]", FunctorTests[Func[Option, Int, ?]].functor[Int, Int, Int])
    checkAll("Functor[Func[Option, Int, ?]]", SerializableTests.serializable(Functor[Func[Option, Int, ?]]))
  }

  {
    implicit val appFuncApp = AppFunc.appFuncApplicative[Option, Int]
    implicit val iso = CartesianTests.Isomorphisms.invariant[AppFunc[Option, Int, ?]]
    checkAll("AppFunc[Option, Int, Int]", ApplicativeTests[AppFunc[Option, Int, ?]].applicative[Int, Int, Int])
    checkAll("Applicative[AppFunc[Option, Int, ?]]", SerializableTests.serializable(Applicative[AppFunc[Option, Int, ?]]))
  }

  test("product") {
    val f = appFunc { (x: Int) => (Some(x + 10): Option[Int]) }
    val g = appFunc { (x: Int) => List(x * 2) }
    val h = f product g
    val x = h.run(1)
    (x.first, x.second) should === ((Some(11), List(2)))
  }

  test("traverse") {
    val f = Func.appFunc { (x: Int) => (Some(x + 10): Option[Int]) }
    val xs = f traverse List(1, 2, 3)
    xs should === (Some(List(11, 12, 13)))
  }
}
