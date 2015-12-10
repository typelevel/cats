package cats
package tests

import cats.laws.{CoflatMapLaws, FlatMapLaws}
import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests, MonoidalTests}
import cats.laws.discipline.eq._

class OptionTests extends CatsSuite {
  checkAll("Option[Int]", MonoidalTests[Option].monoidal[Int, Int, Int])
  checkAll("Monoidal[Option]", SerializableTests.serializable(Monoidal[Option]))

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

  // The following two tests check the kleisliAssociativity and
  // cokleisliAssociativity laws which are a different formulation of
  // the flatMapAssociativity and coflatMapAssociativity laws. Since
  // these laws are more or less duplicates of existing laws, we don't
  // check them for all types that have FlatMap or CoflatMap instances.

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
}
