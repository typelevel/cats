package cats
package tests

import cats.laws.discipline.{CoflatMapTests, MonadTests, AlternativeTests, SerializableTests, TraverseTests, SemigroupalTests}

class StreamTests extends CatsSuite {
  checkAll("Stream[Int]", SemigroupalTests[Stream].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Stream]", SerializableTests.serializable(Semigroupal[Stream]))

  checkAll("Stream[Int]", CoflatMapTests[Stream].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Stream]", SerializableTests.serializable(CoflatMap[Stream]))

  checkAll("Stream[Int]", AlternativeTests[Stream].alternative[Int, Int, Int])
  checkAll("Alternative[Stream]", SerializableTests.serializable(Alternative[Stream]))

  checkAll("Stream[Int]", MonadTests[Stream].monad[Int, Int, Int])
  checkAll("Monad[Stream]", SerializableTests.serializable(Monad[Stream]))

  checkAll("Stream[Int] with Option", TraverseTests[Stream].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[Stream]", SerializableTests.serializable(Traverse[Stream]))

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
