package cats
package tests

import cats.data.ZipStream
import cats.laws.discipline.{AlternativeTests, ApplyTests, CartesianTests, CoflatMapTests, MonadTests, SerializableTests, TraverseTests}
import cats.laws.discipline.arbitrary._

class StreamTests extends CatsSuite {
  checkAll("Stream[Int]", CartesianTests[Stream].cartesian[Int, Int, Int])
  checkAll("Cartesian[Stream]", SerializableTests.serializable(Cartesian[Stream]))

  checkAll("Stream[Int]", CoflatMapTests[Stream].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Stream]", SerializableTests.serializable(CoflatMap[Stream]))

  checkAll("Stream[Int]", AlternativeTests[Stream].alternative[Int, Int, Int])
  checkAll("Alternative[Stream]", SerializableTests.serializable(Alternative[Stream]))

  checkAll("Stream[Int]", MonadTests[Stream].monad[Int, Int, Int])
  checkAll("Monad[Stream]", SerializableTests.serializable(Monad[Stream]))

  checkAll("Stream[Int] with Option", TraverseTests[Stream].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[Stream]", SerializableTests.serializable(Traverse[Stream]))

  // Can't test applicative laws as they don't terminate
  checkAll("ZipStream[Int]", ApplyTests[ZipStream].apply[Int, Int, Int])

  test("show") {
    Stream(1, 2, 3).show should === ("Stream(1, ?)")
    Stream.empty[Int].show should === ("Stream()")
  }

  test("Show[Stream] is referentially transparent, unlike Stream.toString") {
    forAll { stream: Stream[Int] =>
      if (!stream.isEmpty) {
        val unevaluatedStream = stream map identity
        val initialShow = unevaluatedStream.show

        // Evaluating the tail can cause Stream.toString to return different values,
        // depending on the internal state of the Stream. Show[Stream] should return
        // consistent values independent of internal state.
        unevaluatedStream.tail
        initialShow should === (unevaluatedStream.show)
      } else {
        stream.show should === (stream.toString)
      }
    }
  }

}
