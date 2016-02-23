package cats
package tests

import cats.laws.{ApplicativeLaws, CoflatMapLaws, FlatMapLaws, MonadLaws}
import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests, CartesianTests}
import cats.laws.discipline.eq._

class OptionTests extends CatsSuite {
  checkAll("Option[Int]", CartesianTests[Option].cartesian[Int, Int, Int])
  checkAll("Cartesian[Option]", SerializableTests.serializable(Cartesian[Option]))

  checkAll("Option[Int]", CoflatMapTests[Option].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Option]", SerializableTests.serializable(CoflatMap[Option]))

  checkAll("Option[Int]", MonadCombineTests[Option].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Option]", SerializableTests.serializable(MonadCombine[Option]))

  checkAll("Option[Int] with Option", TraverseTests[Option].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Option]", SerializableTests.serializable(Traverse[Option]))

  test("show") {
    none[Int].show should === ("None")
    1.some.show should === ("Some(1)")

    forAll { fs: Option[String] =>
      fs.show should === (fs.toString)
    }
  }

  // The following tests check laws which are a different formulation of
  // laws that are checked. Since these laws are more or less duplicates of
  // existing laws, we don't check them for all types that have the relevant
  // instances.

  test("Kleisli associativity") {
    forAll { (l: Long,
              f: Long => Option[Int],
              g: Int  => Option[Char],
              h: Char => Option[String]) =>
      val isEq = FlatMapLaws[Option].kleisliAssociativity(f, g, h, l)
      isEq.lhs should === (isEq.rhs)
    }
  }

  test("Cokleisli associativity") {
    forAll { (l: Option[Long],
              f: Option[Long] => Int,
              g: Option[Int]  => Char,
              h: Option[Char] => String) =>
      val isEq = CoflatMapLaws[Option].cokleisliAssociativity(f, g, h, l)
      isEq.lhs should === (isEq.rhs)
    }
  }

  test("applicative composition") {
    forAll { (fa: Option[Int],
              fab: Option[Int => Long],
              fbc: Option[Long => Char]) =>
      val isEq = ApplicativeLaws[Option].applicativeComposition(fa, fab, fbc)
      isEq.lhs should === (isEq.rhs)
    }
  }

  val monadLaws = MonadLaws[Option]

  test("Kleisli left identity") {
    forAll { (a: Int, f: Int => Option[Long]) =>
      val isEq = monadLaws.kleisliLeftIdentity(a, f)
      isEq.lhs should === (isEq.rhs)
    }
  }

  test("Kleisli right identity") {
    forAll { (a: Int, f: Int => Option[Long]) =>
      val isEq = monadLaws.kleisliRightIdentity(a, f)
      isEq.lhs should === (isEq.rhs)
    }
  }
}
