package cats.tests

import cats.{Alternative, CoflatMap, CommutativeMonad, Eval, Later, MonadError, Semigroupal, Traverse, TraverseFilter}
import cats.instances.all._
import cats.laws.{ApplicativeLaws, CoflatMapLaws, FlatMapLaws, MonadLaws}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.all._

class OptionSuite extends CatsSuite {
  checkAll("Option[Int]", SemigroupalTests[Option].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Option]", SerializableTests.serializable(Semigroupal[Option]))

  checkAll("Option[Int]", CoflatMapTests[Option].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Option]", SerializableTests.serializable(CoflatMap[Option]))

  checkAll("Option[Int]", AlternativeTests[Option].alternative[Int, Int, Int])
  checkAll("Alternative[Option]", SerializableTests.serializable(Alternative[Option]))

  checkAll("Option[Int]", CommutativeMonadTests[Option].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[Option]", SerializableTests.serializable(CommutativeMonad[Option]))

  checkAll("Option[Int] with Option", TraverseTests[Option].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Option]", SerializableTests.serializable(Traverse[Option]))

  checkAll("Option[Int] with Option", TraverseFilterTests[Option].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Option]", SerializableTests.serializable(TraverseFilter[Option]))

  checkAll("Option with Unit", MonadErrorTests[Option, Unit].monadError[Int, Int, Int])
  checkAll("MonadError[Option, Unit]", SerializableTests.serializable(MonadError[Option, Unit]))

  test("show") {
    none[Int].show should ===("None")
    1.some.show should ===("Some(1)")

    forAll { (fs: Option[String]) =>
      fs.show should ===(fs.toString)
    }
  }

  // The following tests check laws which are a different formulation of
  // laws that are checked. Since these laws are more or less duplicates of
  // existing laws, we don't check them for all types that have the relevant
  // instances.

  test("Kleisli associativity") {
    forAll { (l: Long, f: Long => Option[Int], g: Int => Option[Char], h: Char => Option[String]) =>
      val isEq = FlatMapLaws[Option].kleisliAssociativity(f, g, h, l)
      isEq.lhs should ===(isEq.rhs)
    }
  }

  test("Cokleisli associativity") {
    forAll { (l: Option[Long], f: Option[Long] => Int, g: Option[Int] => Char, h: Option[Char] => String) =>
      val isEq = CoflatMapLaws[Option].cokleisliAssociativity(f, g, h, l)
      isEq.lhs should ===(isEq.rhs)
    }
  }

  test("applicative composition") {
    forAll { (fa: Option[Int], fab: Option[Int => Long], fbc: Option[Long => Char]) =>
      val isEq = ApplicativeLaws[Option].applicativeComposition(fa, fab, fbc)
      isEq.lhs should ===(isEq.rhs)
    }
  }

  val monadLaws = MonadLaws[Option]

  test("Kleisli left identity") {
    forAll { (a: Int, f: Int => Option[Long]) =>
      val isEq = monadLaws.kleisliLeftIdentity(a, f)
      isEq.lhs should ===(isEq.rhs)
    }
  }

  test("Kleisli right identity") {
    forAll { (a: Int, f: Int => Option[Long]) =>
      val isEq = monadLaws.kleisliRightIdentity(a, f)
      isEq.lhs should ===(isEq.rhs)
    }
  }

  // OptionIdOps tests

  test(".some with null argument still results in Some #871") {
    val s: String = null
    // can't use `s.some should === (Some(null))` here, because it leads to NullPointerException
    s.some.exists(_ == null) should ===(true)
  }

  test("map2Eval is lazy") {
    val bomb: Eval[Option[Int]] = Later(sys.error("boom"))
    none[Int].map2Eval(bomb)(_ + _).value should ===(None)
  }

  test("toOptionT consistency") {
    List(false) should ===(1.some.toOptionT[List].isEmpty)
    List(true) should ===(Option.empty[Int].toOptionT[List].isEmpty)
  }
}
