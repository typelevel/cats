package cats
package tests

import cats.laws.discipline.{CoflatMapTests, MonadCombineTests, SerializableTests}

class OptionTests extends CatsSuite {
  checkAll("Option[Int]", CoflatMapTests[Option].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Option]", SerializableTests.serializable(CoflatMap[Option]))

  checkAll("Option[Int]", MonadCombineTests[Option].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Option]", SerializableTests.serializable(MonadCombine[Option]))
}
