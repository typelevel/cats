package cats.tests

import cats.laws.discipline.{CoflatMapTests, MonadCombineTests, AlternativeTests}

class OptionTests extends CatsSuite {
  checkAll("Option[Int]", CoflatMapTests[Option].coflatMap[Int, Int, Int])
  checkAll("Option[Int]", MonadCombineTests[Option].monadCombine[Int, Int, Int])
  checkAll("Option[Int]", AlternativeTests[Option].alternative[Int, String, Int])
}
