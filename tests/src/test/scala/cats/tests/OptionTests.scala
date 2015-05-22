package cats
package tests

import cats.laws.discipline.{CoflatMapTests, MonadCombineTests, SerializableTests}

class OptionTests extends CatsSuite {
  checkAll("CoflatMap[Option[Int]]", CoflatMapTests[Option].coflatMap[Int, Int, Int])
  checkAll("Serializable[CoflatMap[Option]]", SerializableTests.serializable(CoflatMap[Option]))

  checkAll("MonadCombine[Option[Int]]", MonadCombineTests[Option].monadCombine[Int, Int, Int])
  checkAll("Serializable[MonadCombine[Option[?]]]", SerializableTests.serializable(MonadCombine[Option]))
}
