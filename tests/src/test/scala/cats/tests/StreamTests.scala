package cats.tests

import cats.laws.discipline.MonoidKTests

class StreamTests extends CatsSuite {
  checkAll("Stream[Int]", MonoidKTests[Stream].monoidK[Int])
}
