package cats.tests

import cats.laws.discipline.{CoflatMapTests, MonadCombineTests}

class StreamTests extends CatsSuite {
  checkAll("Stream[Int]", CoflatMapTests[Stream].coflatMap[Int, Int, Int])
  checkAll("Stream[Int]", MonadCombineTests[Stream].monadCombine[Int, Int, Int])
}
