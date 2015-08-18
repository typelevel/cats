package cats
package tests

import cats.data.Streaming
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{TraverseTests, CoflatMapTests, MonadCombineTests, SerializableTests}

class StreamingTests extends CatsSuite {
  checkAll("Streaming[Int]", CoflatMapTests[Streaming].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Streaming]", SerializableTests.serializable(CoflatMap[Streaming]))

  checkAll("Streaming[Int]", MonadCombineTests[Streaming].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Streaming]", SerializableTests.serializable(MonadCombine[Streaming]))

  checkAll("Streaming[Int] with Option", TraverseTests[Streaming].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Streaming]", SerializableTests.serializable(Traverse[Streaming]))
}
