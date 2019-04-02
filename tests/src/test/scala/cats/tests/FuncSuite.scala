package cats
package tests

import cats.data.{AppFunc, Func}
import Func.appFunc
import cats.Contravariant
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._

class FuncSuite extends CatsSuite {
  import cats.laws.discipline.eq._
  implicit def funcEq[F[_], A, B](implicit ev: Eq[A => F[B]]): Eq[Func[F, A, B]] =
    Eq.by[Func[F, A, B], A => F[B]](_.run)
  implicit def appFuncEq[F[_], A, B](implicit ev: Eq[A => F[B]]): Eq[AppFunc[F, A, B]] =
    Eq.by[AppFunc[F, A, B], A => F[B]](_.run)

  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Func[Option, Int, ?]]

  checkAll("Func[Option, MiniInt, Int]", SemigroupalTests[Func[Option, MiniInt, ?]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Func[Option, Int, ?]]", SerializableTests.serializable(Semigroupal[Func[Option, Int, ?]]))

  {
    implicit val catsDataApplicativeForFunc = Func.catsDataApplicativeForFunc[Option, Int]
    checkAll("Func[Option, MiniInt, Int]", ApplicativeTests[Func[Option, MiniInt, ?]].applicative[Int, Int, Int])
    checkAll("Applicative[Func[Option, Int, ?]]", SerializableTests.serializable(Applicative[Func[Option, Int, ?]]))
  }

  {
    implicit val catsDataApplyForFunc = Func.catsDataApplyForFunc[Option, MiniInt]
    checkAll("Func[Option, MiniInt, Int]", ApplyTests[Func[Option, MiniInt, ?]].apply[Int, Int, Int])
    checkAll("Apply[Func[Option, Int, ?]]", SerializableTests.serializable(Apply[Func[Option, Int, ?]]))
  }

  {
    implicit val catsDataFunctorForFunc = Func.catsDataFunctorForFunc[Option, Int]
    checkAll("Func[Option, MiniInt, Int]", FunctorTests[Func[Option, MiniInt, ?]].functor[Int, Int, Int])
    checkAll("Functor[Func[Option, Int, ?]]", SerializableTests.serializable(Functor[Func[Option, Int, ?]]))
  }

  {
    implicit val funcContravariant = Func.catsDataContravariantForFunc[Show, MiniInt]
    checkAll("Func[Show, MiniInt, MiniInt]",
             ContravariantTests[Func[Show, ?, MiniInt]].contravariant[MiniInt, MiniInt, MiniInt])
    checkAll("Contravariant[Func[Show, ?, Int]]", SerializableTests.serializable(Contravariant[Func[Show, ?, Int]]))
  }

  {
    implicit val appFuncApp = AppFunc.appFuncApplicative[Option, Int]
    implicit val iso = SemigroupalTests.Isomorphisms.invariant[AppFunc[Option, Int, ?]]
    checkAll("AppFunc[Option, MiniInt, MiniInt]",
             ApplicativeTests[AppFunc[Option, MiniInt, ?]].applicative[MiniInt, MiniInt, MiniInt])
    checkAll("Applicative[AppFunc[Option, Int, ?]]",
             SerializableTests.serializable(Applicative[AppFunc[Option, Int, ?]]))
  }

  test("product") {
    val f = appFunc { (x: Int) =>
      (Some(x + 10): Option[Int])
    }
    val g = appFunc { (x: Int) =>
      List(x * 2)
    }
    val h = f.product(g)
    val x = h.run(1)
    (x.first, x.second) should ===((Some(11), List(2)))
  }

  test("traverse") {
    val f = Func.appFunc { (x: Int) =>
      (Some(x + 10): Option[Int])
    }
    val xs = f.traverse(List(1, 2, 3))
    xs should ===(Some(List(11, 12, 13)))
  }
}
