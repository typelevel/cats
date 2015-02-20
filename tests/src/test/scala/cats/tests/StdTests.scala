package cats.tests

import cats.laws.discipline.{ComonadTests, MonadTests, MonadFilterTests, MonoidKTests, AlternativeTests}

class StdTests extends CatsSuite {
  checkAll("Function0[Int]", ComonadTests[Function0].comonad[Int, Int, Int])
  checkAll("Function0[Int]", MonadTests[Function0].monad[Int, Int, Int])
  checkAll("Option[Int]", MonadFilterTests[Option].monadFilter[Int, Int, Int])
  checkAll("Option[String]", MonadFilterTests[Option].monadFilter[String, Int, Int])
  checkAll("Option[Int]", AlternativeTests[Option].alternative[Int, String, Int])
  checkAll("List[Int]", MonadFilterTests[List].monadFilter[Int, Int, Int])
  checkAll("List[Int]", MonoidKTests[List].monoidK[Int])
  checkAll("Stream[Int]", MonoidKTests[Stream].monoidK[Int])
  checkAll("Vector[Int]", MonoidKTests[Vector].monoidK[Int])
}
