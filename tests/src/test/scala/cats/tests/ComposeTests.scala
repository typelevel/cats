package cats
package tests

import cats.data.{ NonEmptyList, NonEmptyVector }
import cats.laws.discipline.{ AlternativeTests, ApplicativeTests, FoldableTests, CartesianTests, MonoidKTests, SemigroupKTests, arbitrary }, arbitrary._

class ComposeTests extends CatsSuite {
  // we have a lot of generated lists of lists in these tests. We have to tell
  // Scalacheck to calm down a bit so we don't hit memory and test duration
  // issues.
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 5, minSuccessful = 20)

  {
    // Alternative composition

    implicit val alternativeListVector: Alternative[Lambda[A => List[Vector[A]]]] = Alternative[List] compose Alternative[Vector]
    implicit val iso = CartesianTests.Isomorphisms.invariant[Lambda[A => List[Vector[A]]]]

    checkAll("Alternative[Lambda[A => List[Vector[A]]]]", AlternativeTests[Lambda[A => List[Vector[A]]]].alternative[Int, Int, Int])
  }

  {
    // Applicative composition

    implicit val applicativeListVector: Applicative[Lambda[A => List[Vector[A]]]] = Applicative[List] compose Applicative[Vector]
    implicit val iso = CartesianTests.Isomorphisms.invariant[Lambda[A => List[Vector[A]]]]

    checkAll("Applicative[Lambda[A => List[Vector[A]]]]", ApplicativeTests[Lambda[A => List[Vector[A]]]].applicative[Int, Int, Int])
  }

  {
    // Foldable composition

    implicit val foldableListVector: Foldable[Lambda[A => List[Vector[A]]]] = Foldable[List] compose Foldable[Vector]

    checkAll("Foldable[Lambda[A => List[Vector[A]]]]", FoldableTests[Lambda[A => List[Vector[A]]]].foldable[Int, Int])
  }

  {
    // MonoidK composition

    implicit val monoidKListVector: MonoidK[Lambda[A => List[Vector[A]]]] = MonoidK[List].composeK[Vector]

    checkAll("MonoidK[Lambda[A => List[Vector[A]]]]", MonoidKTests[Lambda[A => List[Vector[A]]]].monoidK[Int])
  }

  {
    // Reducible composition

    implicit val reducibleListVector: Reducible[Lambda[A => NonEmptyList[NonEmptyVector[A]]]] =
      Reducible[NonEmptyList] compose Reducible[NonEmptyVector]

    // No Reducible-specific laws, so check the Foldable laws are satisfied
    checkAll("Reducible[Lambda[A => List[Vector[A]]]]", FoldableTests[Lambda[A => NonEmptyList[NonEmptyVector[A]]]].foldable[Int, Int])
  }

  {
    // SemigroupK composition

    implicit val semigroupKListVector: SemigroupK[Lambda[A => List[Vector[A]]]] = SemigroupK[List].composeK[Vector]

    checkAll("SemigroupK[Lambda[A => List[Vector[A]]]]", SemigroupKTests[Lambda[A => List[Vector[A]]]].semigroupK[Int])
  }
}
