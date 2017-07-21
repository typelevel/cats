package cats
package tests

import cats.data.NonEmptyList
import cats.laws.discipline.{TraverseTests, CoflatMapTests, AlternativeTests, SerializableTests, CartesianTests}
import cats.laws.discipline.arbitrary._

class ListTests extends CatsSuite {

  checkAll("List[Int]", CartesianTests[List].cartesian[Int, Int, Int])
  checkAll("Cartesian[List]", SerializableTests.serializable(Cartesian[List]))

  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[List]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("List[Int]", AlternativeTests[List].alternative[Int, Int, Int])
  checkAll("Alternative[List]", SerializableTests.serializable(Alternative[List]))

  checkAll("List[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[List]", SerializableTests.serializable(Traverse[List]))

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
      fa.groupByNel(f).mapValues(_.toList) should === (fa.groupBy(f))
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
