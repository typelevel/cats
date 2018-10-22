package cats

import cats.laws.discipline.InjectTests
import cats.tests.CatsSuite

class InjectSuite extends CatsSuite {

  type StringOrInt = Either[String, Int]

  test("inj & prj") {
    def distr[F](f1: F, f2: F)(implicit
                               I0: Inject[String, F],
                               I1: Inject[Int, F]): Option[String] =
      for {
        x <- I0.prj(f1)
        y <- I1.prj(f2)
      } yield s"$x $y"

    forAll { (x: String, y: Int) =>
      val expr1: StringOrInt = Inject[String, StringOrInt].inj(x)
      val expr2: StringOrInt = Inject[Int, StringOrInt].inj(y)
      val res = distr(expr1, expr2)
      res should ===(Some(s"$x $y"))
    }
  }

  test("apply & unapply") {
    def distr[F](f1: F, f2: F)(implicit
                               I0: Inject[String, F],
                               I1: Inject[Int, F]): Option[String] =
      for {
        x <- I0.unapply(f1)
        y <- I1.unapply(f2)
      } yield s"$x $y"

    forAll { (x: String, y: Int) =>
      val expr1: StringOrInt = Inject[String, StringOrInt].apply(x)
      val expr2: StringOrInt = Inject[Int, StringOrInt].apply(y)
      val res = distr(expr1, expr2)
      res should ===(Some(s"$x $y"))
    }
  }

  test("apply in left") {
    forAll { (y: String) =>
      Inject[String, StringOrInt].inj(y) == Left(y) should ===(true)
    }
  }

  test("apply in right") {
    forAll { (y: Int) =>
      Inject[Int, StringOrInt].inj(y) == Right(y) should ===(true)
    }
  }

  test("null identity") {
    val stringNull = null.asInstanceOf[String]
    Inject.catsReflexiveInjectInstance[String].inj(stringNull) should ===(stringNull)
    Inject.catsReflexiveInjectInstance[String].prj(stringNull) should ===(Some(stringNull))
  }

  checkAll("Inject[String, StringOrInt]", InjectTests[String, StringOrInt].inject)
  checkAll("Inject[Int, StringOrInt]", InjectTests[Int, StringOrInt].inject)
}
