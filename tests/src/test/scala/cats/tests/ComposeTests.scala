package cats
package tests

import cats.data.{ NonEmptyList, NonEmptyVector, OneAnd }
import cats.laws.discipline.{ ApplicativeTests, FoldableTests, MonoidalTests, SemigroupKTests, arbitrary, eq }, arbitrary._, eq._
import org.scalacheck.Arbitrary

class ComposeTests extends CatsSuite {
  // we have a lot of generated lists of lists in these tests. We have to tell
  // Scalacheck to calm down a bit so we don't hit memory and test duration
  // issues.
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 5, minSuccessful = 20)

  {
    // Applicative composition

    implicit val applicativeListVector: Applicative[Lambda[A => List[Vector[A]]]] = Applicative[List] compose Applicative[Vector]
    implicit val iso = MonoidalTests.Isomorphisms.invariant[Lambda[A => List[Vector[A]]]]

    checkAll("Applicative[Lambda[A => List[Vector[A]]]]", ApplicativeTests[Lambda[A => List[Vector[A]]]].applicative[Int, Int, Int])
  }

  {
    // Foldable composition

    implicit val foldableListVector: Foldable[Lambda[A => List[Vector[A]]]] = Foldable[List] compose Foldable[Vector]

    checkAll("Foldable[Lambda[A => List[Vector[A]]]]", FoldableTests[Lambda[A => List[Vector[A]]]].foldable[Int, Int])
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

    implicit val semigroupKListVector: SemigroupK[Lambda[A => List[Vector[A]]]] = SemigroupK[List] compose SemigroupK[Vector]

    checkAll("SemigroupK[Lambda[A => List[Vector[A]]]]", SemigroupKTests[Lambda[A => List[Vector[A]]]].semigroupK[Int])
  }
}
