package cats.tests

import cats.{Bifunctor, Functor}
import cats.laws.discipline.{BifunctorTests, FunctorTests, SerializableTests}

class BifunctorSuite extends CatsSuite {
  type Tuple2Either[A, B] = (Either[A, B], Either[A, B])
  val tuple2ComposeEither: Bifunctor[Tuple2Either] =
    Bifunctor[Tuple2].compose[Either]

  checkAll("Tuple2 compose Either",
           BifunctorTests(tuple2ComposeEither).bifunctor[Int, Int, Int, String, String, String]
  )
  checkAll("Bifunctor[Tuple2 compose Either]", SerializableTests.serializable(tuple2ComposeEither))

  {
    type LeftFunctor[A] = (Either[A, Int], Either[A, Int])
    implicit val leftFunctor: Functor[LeftFunctor] = tuple2ComposeEither.leftFunctor
    checkAll("Bifunctor[Tuple2 compose Either].leftFunctor", FunctorTests[LeftFunctor].functor[Int, Int, Int])
  }

  {
    type RightFunctor[A] = (Either[Int, A], Either[Int, A])
    implicit val leftFunctor: Functor[RightFunctor] = tuple2ComposeEither.rightFunctor
    checkAll("Bifunctor[Tuple2 compose Either].rightFunctor", FunctorTests[RightFunctor].functor[Int, Int, Int])
  }
}
