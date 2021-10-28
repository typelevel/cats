package cats.tests

import cats.NonEmptyAlternative
import cats.laws.discipline.NonEmptyAlternativeTests

class NonEmptyAlternativeSuite extends CatsSuite {
  implicit val listWrapperNeAlternative: NonEmptyAlternative[ListWrapper] = ListWrapper.nonEmptyAlternative

  checkAll("Option[Int]", NonEmptyAlternativeTests[Option].nonEmptyAlternative[Int, Int, Int])
  checkAll("List[Int]", NonEmptyAlternativeTests[List].nonEmptyAlternative[Int, Int, Int])
  checkAll("ListWrapper[List[Int]]", NonEmptyAlternativeTests[ListWrapper].nonEmptyAlternative[Int, Int, Int])
  checkAll(
    "compose ListWrapper[List[Int]]",
    NonEmptyAlternativeTests.composed[ListWrapper, List].nonEmptyAlternative[Int, Int, Int]
  )
  checkAll(
    "compose List[ListWrapper[Int]]",
    NonEmptyAlternativeTests.composed[List, ListWrapper].nonEmptyAlternative[Int, Int, Int]
  )
  checkAll(
    "compose ListWrapper[ListWrapper[Int]]",
    NonEmptyAlternativeTests.composed[ListWrapper, ListWrapper].nonEmptyAlternative[Int, Int, Int]
  )
}
