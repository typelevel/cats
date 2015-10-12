package cats
package tests

import cats.arrow.NaturalTransformation


class NaturalTransformationTests extends CatsSuite {
  val listToOption =
    new NaturalTransformation[List, Option] {
      def apply[A](fa: List[A]): Option[A] = fa.headOption
    }

  val optionToList =
    new NaturalTransformation[Option, List] {
      def apply[A](fa: Option[A]): List[A] = fa.toList
    }

  test("compose") {
    forAll { (list: List[Int]) =>
      val listToList = optionToList.compose(listToOption)
      listToList(list) should === (list.take(1))
    }
  }

  test("andThen") {
    forAll { (list: List[Int]) =>
      val listToList = listToOption.andThen(optionToList)
      listToList(list) should === (list.take(1))
    }
  }

  test("id is identity") {
    forAll { (list: List[Int]) =>
      NaturalTransformation.id[List].apply(list) should === (list)
    }
  }
}
