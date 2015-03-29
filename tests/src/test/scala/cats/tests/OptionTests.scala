package cats.tests

import cats.{Alternative, CoflatMap, MonadCombine}
import cats.laws.discipline.{AlternativeTests, CoflatMapTests, MonadCombineTests, SerializableTests}

class OptionTests extends CatsSuite {
  checkAll("Option[Int]", AlternativeTests[Option].alternative[Int, String, Int])
  checkAll("Alternative[Option]", SerializableTests.serializable(Alternative[Option]))

  checkAll("Option[Int]", CoflatMapTests[Option].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Option]", SerializableTests.serializable(CoflatMap[Option]))

  checkAll("Option[Int]", MonadCombineTests[Option].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Option]", SerializableTests.serializable(MonadCombine[Option]))
}
