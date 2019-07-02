package cats
package tests

import cats.kernel.laws.discipline.{SerializableTests => _, _}
import cats.laws.discipline._
import org.scalacheck.{Arbitrary, Cogen}

abstract class NonEmptyDataTypeSuite[F[_]: Bimonad : NonEmptyTraverse : SemigroupK]
(name: String)
(implicit PO: Order[F[Int]],
 MF: Semigroup[F[Int]],
 SF: Show[F[Int]],
 EqFABC: Eq[F[(Int, Int, Int)]],
 EqFFA: Eq[F[F[Int]]],
 EqFFFA: cats.kernel.Eq[F[F[F[Int]]]]
) extends CatsSuite {

  implicit def arbitraryFA[A](implicit A: Arbitrary[A]): Arbitrary[F[A]]

  //needed to help compiler recursively find evidence
  implicit def arbitraryFFA[A](implicit A: Arbitrary[A]): Arbitrary[F[F[A]]] = arbitraryFA(arbitraryFA(A))

  implicit def cogenFA[A](implicit A: Cogen[A]): Cogen[F[A]]

  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 20, sizeRange = 5)

  checkAll(s"NonEmpty$name[Int]", OrderTests[F[Int]].order)
  checkAll(s"Order[NonEmpty$name]", SerializableTests.serializable(Order[F[Int]]))

  checkAll(s"NonEmpty$name[Int]", SemigroupTests[F[Int]].semigroup)
  checkAll(s"Semigroup[NonEmpty$name]", SerializableTests.serializable(Semigroup[F[Int]]))

  checkAll(s"Show[NonEmpty$name]", SerializableTests.serializable(Show[F[Int]]))

  checkAll(s"NonEmpty$name[Int] with Option",
    NonEmptyTraverseTests[F].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
  checkAll(s"NonEmptyTraverse[NonEmpty$name[A]]", SerializableTests.serializable(NonEmptyTraverse[F]))


  implicit val iso2 = SemigroupalTests.Isomorphisms.invariant[F](Bimonad[F])
  checkAll(s"NonEmpty$name[Int]", SemigroupKTests[F].semigroupK[Int])
  checkAll(s"SemigroupK[NonEmpty$name[Int]]", SerializableTests.serializable(SemigroupK[F]))


  checkAll(s"NonEmpty$name[Int]", BimonadTests[F].bimonad[Int, Int, Int])
  checkAll("Bimonad[F[A]]", SerializableTests.serializable(Bimonad[F]))

}