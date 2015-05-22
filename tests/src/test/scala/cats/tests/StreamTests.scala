package cats
package tests

import cats.laws.discipline.{CoflatMapTests, MonadCombineTests, SerializableTests}

class StreamTests extends CatsSuite {
  checkAll("CoflatMap[Stream[Int]]", CoflatMapTests[Stream].coflatMap[Int, Int, Int])
  checkAll("Serializable[CoflatMap[Stream]]", SerializableTests.serializable(CoflatMap[Stream]))

  checkAll("MonadCombine[Stream[Int]]", MonadCombineTests[Stream].monadCombine[Int, Int, Int])
  checkAll("Serializable[MonadCombine[Stream]]", SerializableTests.serializable(MonadCombine[Stream]))
}
