package cats
package tests

import cats.laws.{ApplicativeLaws, CoflatMapLaws, FlatMapLaws, MonadLaws}
import cats.laws.discipline._

class OptionTests extends CatsSuite {
  checkAll("Option[Int]", CartesianTests[Option].cartesian[Int, Int, Int])
  checkAll("Cartesian[Option]", SerializableTests.serializable(Cartesian[Option]))

  checkAll("Option[Int]", CoflatMapTests[Option].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Option]", SerializableTests.serializable(CoflatMap[Option]))

  checkAll("Option[Int]", MonadCombineTests[Option].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Option]", SerializableTests.serializable(MonadCombine[Option]))

  checkAll("Option[Int]", MonadRecTests[Option].monadRec[Int, Int, Int])
  checkAll("MonadRec[Option]", SerializableTests.serializable(MonadRec[Option]))

  checkAll("Option[Int] with Option", TraverseTests[Option].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Option]", SerializableTests.serializable(Traverse[Option]))

  checkAll("Option with Unit", MonadErrorTests[Option, Unit].monadError[Int, Int, Int])
  checkAll("MonadError[Option, Unit]", SerializableTests.serializable(MonadError[Option, Unit]))

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

  // OptionIdOps tests

  test(".some with null argument still results in Some #871") {
    val s: String = null
    // can't use `s.some should === (Some(null))` here, because it leads to NullPointerException
    s.some.exists(_ == null) should ===(true)
  }

  test("map2Eval is lazy") {
    val bomb: Eval[Option[Int]] = Later(sys.error("boom"))
    none[Int].map2Eval(bomb)(_ + _).value should === (None)
  }
}
