package cats
package tests

import cats.data.{NonEmptyList, ZipList}
import cats.laws.discipline.{CommutativeApplyTests, TraverseTests, CoflatMapTests, AlternativeTests, MonadTests, SerializableTests, SemigroupalTests}
import cats.laws.discipline.arbitrary._

class ListSuite extends CatsSuite {

  checkAll("List[Int]", SemigroupalTests[List].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[List]", SerializableTests.serializable(Semigroupal[List]))

  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[List]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("List[Int]", AlternativeTests[List].alternative[Int, Int, Int])
  checkAll("Alternative[List]", SerializableTests.serializable(Alternative[List]))

  checkAll("List[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[List]", SerializableTests.serializable(Traverse[List]))

  checkAll("List[Int]", MonadTests[List].monad[Int, Int, Int])
  checkAll("Monad[List]", SerializableTests.serializable(Monad[List]))

  checkAll("ZipList[Int]", CommutativeApplyTests[ZipList].commutativeApply[Int, Int, Int])

  test("nel => list => nel returns original nel")(
    forAll { fa: NonEmptyList[Int] =>
      fa.toList.toNel should === (Some(fa))
    }
  )

  test("toNel on empty list returns None"){
    List.empty[Int].toNel should === (None)
  }

  test("groupByNel should be consistent with groupBy")(
    forAll { (fa: List[Int], f: Int => Int) =>
      fa.groupByNel(f).map{ case (k, v) => (k, v.toList)} should === (fa.groupBy(f))
    }
  )

  test("show"){
    List(1, 2, 3).show should === ("List(1, 2, 3)")
    (Nil: List[Int]).show should === ("List()")
    forAll { l: List[String] =>
      l.show should === (l.toString)
    }
  }
}
