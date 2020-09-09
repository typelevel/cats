package cats.tests

import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.{ApplicativeLaws, CoflatMapLaws, FlatMapLaws, MonadLaws}
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.show._
import cats.{
  Align,
  Alternative,
  CoflatMap,
  CommutativeMonad,
  Eval,
  Later,
  MonadError,
  Semigroupal,
  Traverse,
  TraverseFilter
}
import cats.syntax.eq._
import org.scalacheck.Prop._

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

  checkAll("Option[Int]", AlignTests[Option].align[Int, Int, Int, Int])
  checkAll("Align[Option]", SerializableTests.serializable(Align[Option]))

  test("show") {
    assert(none[Int].show === "None")
    assert(1.some.show === "Some(1)")

    forAll { (fs: Option[String]) =>
      assert(fs.show === (fs.toString))
    }
  }

  // The following tests check laws which are a different formulation of
  // laws that are checked. Since these laws are more or less duplicates of
  // existing laws, we don't check them for all types that have the relevant
  // instances.

  test("Kleisli associativity") {
    forAll { (l: Long, f: Long => Option[Int], g: Int => Option[Char], h: Char => Option[String]) =>
      val isEq = FlatMapLaws[Option].kleisliAssociativity(f, g, h, l)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  test("Cokleisli associativity") {
    forAll { (l: Option[Long], f: Option[Long] => Int, g: Option[Int] => Char, h: Option[Char] => String) =>
      val isEq = CoflatMapLaws[Option].cokleisliAssociativity(f, g, h, l)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  test("applicative composition") {
    forAll { (fa: Option[Int], fab: Option[Int => Long], fbc: Option[Long => Char]) =>
      val isEq = ApplicativeLaws[Option].applicativeComposition(fa, fab, fbc)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  val monadLaws = MonadLaws[Option]

  test("Kleisli left identity") {
    forAll { (a: Int, f: Int => Option[Long]) =>
      val isEq = monadLaws.kleisliLeftIdentity(a, f)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  test("Kleisli right identity") {
    forAll { (a: Int, f: Int => Option[Long]) =>
      val isEq = monadLaws.kleisliRightIdentity(a, f)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  // OptionIdOps tests

  test(".some with null argument still results in Some #871") {
    val s: String = null
    // can't use `s.some ===  (Some(null))` here, because it leads to NullPointerException)
    assert(s.some.exists(_ == null) === true)
  }

  test("map2Eval is lazy") {
    val bomb: Eval[Option[Int]] = Later(sys.error("boom"))
    assert(none[Int].map2Eval(bomb)(_ + _).value === None)
  }

  test("toOptionT consistency") {
    assert(List(false) === (1.some.toOptionT[List].isEmpty))
    assert(List(true) === (Option.empty[Int].toOptionT[List].isEmpty))
  }
}
