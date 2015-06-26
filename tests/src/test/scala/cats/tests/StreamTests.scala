package cats
package tests

import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests}

class StreamTests extends CatsSuite {
  checkAll("Stream[Int]", CoflatMapTests[Stream].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Stream]", SerializableTests.serializable(CoflatMap[Stream]))

  checkAll("Stream[Int]", MonadCombineTests[Stream].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Stream]", SerializableTests.serializable(MonadCombine[Stream]))

  checkAll("Stream[Int] with Option", TraverseTests[Stream].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Stream]", SerializableTests.serializable(Traverse[Stream]))
}
