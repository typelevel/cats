package cats
package tests

import scala.collection.immutable.Queue

import cats.laws.discipline.{CoflatMapTests, MonadTests, AlternativeTests, SerializableTests, TraverseTests, CartesianTests}

class QueueTests extends CatsSuite {
  checkAll("Queue[Int]", CartesianTests[Queue].cartesian[Int, Int, Int])
  checkAll("Cartesian[Queue]", SerializableTests.serializable(Cartesian[Queue]))

  checkAll("Queue[Int]", CoflatMapTests[Queue].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Queue]", SerializableTests.serializable(CoflatMap[Queue]))

  checkAll("Queue[Int]", AlternativeTests[Queue].alternative[Int, Int, Int])
  checkAll("Alternative[Queue]", SerializableTests.serializable(Alternative[Queue]))

  checkAll("Queue[Int]", MonadTests[Queue].monad[Int, Int, Int])
  checkAll("Monad[Queue]", SerializableTests.serializable(Monad[Queue]))

  checkAll("Queue[Int] with Option", TraverseTests[Queue].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[Queue]", SerializableTests.serializable(Traverse[Queue]))

  test("show") {
    Queue(1, 2, 3).show should === ("Queue(1, 2, 3)")
    Queue.empty[Int].show should === ("Queue()")
  }
}
