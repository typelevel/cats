package cats
package tests

import cats.arrow.NaturalTransformation

import org.scalacheck.Prop.forAll

class NaturalTransformationTests extends CatsSuite {
  val listToOption =
    new NaturalTransformation[List, Option] {
      def apply[A](fa: List[A]): Option[A] = fa.headOption
    }

  val optionToList =
    new NaturalTransformation[Option, List] {
      def apply[A](fa: Option[A]): List[A] = fa.toList
    }

  test("compose")(check {
    forAll { (list: List[Int]) =>
      val listToList = optionToList.compose(listToOption)
      listToList(list) == list.take(1)
    }
  })

  test("andThen")(check {
    forAll { (list: List[Int]) =>
      val listToList = listToOption.andThen(optionToList)
      listToList(list) == list.take(1)
    }
  })

  test("id is identity")(check {
    forAll { (list: List[Int]) =>
      NaturalTransformation.id[List].apply(list) == list
    }
  })
}
