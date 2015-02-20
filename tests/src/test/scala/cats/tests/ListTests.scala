package cats.tests

import cats.laws.discipline.{CoflatMapTests, MonadCombineTests}

class ListTests extends CatsSuite {
  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("List[Int]", MonadCombineTests[List].monadCombine[Int, Int, Int])
}
