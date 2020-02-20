package cats.tests

import cats.{Alternative, CoflatMap, Monad, Semigroupal, Traverse, TraverseFilter}
import cats.instances.all._
import cats.laws.discipline.{
  AlternativeTests,
  CoflatMapTests,
  MonadTests,
  SemigroupalTests,
  SerializableTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.syntax.show._
import scala.collection.immutable.Queue

class QueueSuite extends CatsSuite {
  checkAll("Queue[Int]", SemigroupalTests[Queue].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Queue]", SerializableTests.serializable(Semigroupal[Queue]))

  checkAll("Queue[Int]", CoflatMapTests[Queue].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Queue]", SerializableTests.serializable(CoflatMap[Queue]))

  checkAll("Queue[Int]", AlternativeTests[Queue].alternative[Int, Int, Int])
  checkAll("Alternative[Queue]", SerializableTests.serializable(Alternative[Queue]))

  checkAll("Queue[Int]", MonadTests[Queue].monad[Int, Int, Int])
  checkAll("Monad[Queue]", SerializableTests.serializable(Monad[Queue]))

  checkAll("Queue[Int] with Option", TraverseTests[Queue].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[Queue]", SerializableTests.serializable(Traverse[Queue]))

  checkAll("Queue[Int]", TraverseFilterTests[Queue].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Queue]", SerializableTests.serializable(TraverseFilter[Queue]))

  test("show") {
    Queue(1, 2, 3).show should ===("Queue(1, 2, 3)")
    Queue.empty[Int].show should ===("Queue()")
  }
}
