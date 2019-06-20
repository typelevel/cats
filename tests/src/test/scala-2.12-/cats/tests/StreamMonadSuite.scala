package cats
package tests

import laws.discipline.MonadTests
import data.NonEmptyStream
import cats.laws.discipline.arbitrary._


class StreamMonadSuite extends CatsSuite {
  checkAll("Stream[Int]", MonadTests[Stream].monad[Int, Int, Int])
  checkAll("NonEmptyStream[Int]", MonadTests[NonEmptyStream].monad[Int, Int, Int])


}
