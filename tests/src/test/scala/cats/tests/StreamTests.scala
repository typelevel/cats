package cats
package tests

import cats.laws.discipline.{CoflatMapTests, MonadCombineTests, SerializableTests, TraverseTests, MonoidalTests}
import cats.laws.discipline.eq.tuple3Eq

class StreamTests extends CatsSuite {
  checkAll("Stream[Int]", MonoidalTests[Stream].monoidal[Int, Int, Int])
  checkAll("Monoidal[Stream]", SerializableTests.serializable(Monoidal[Stream]))

  checkAll("Stream[Int]", CoflatMapTests[Stream].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Stream]", SerializableTests.serializable(CoflatMap[Stream]))

  checkAll("Stream[Int]", MonadCombineTests[Stream].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Stream]", SerializableTests.serializable(MonadCombine[Stream]))

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
