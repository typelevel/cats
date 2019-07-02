package cats
package tests

import cats.kernel.laws.discipline.{SerializableTests => _, _}
import cats.laws.discipline._
import org.scalacheck.{Arbitrary, Cogen}

abstract class NonEmptyDataTypeSuite[F[_]: Bimonad : NonEmptyTraverse : Alternative: TraverseFilter]
(name: String)
(implicit PO: Order[F[Int]],
 MF: Monoid[F[Int]],
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

  checkAll(s"NonEmpty$name[Int]", MonoidTests[F[Int]].monoid)
  checkAll(s"Monoid[NonEmpty$name]", SerializableTests.serializable(Monoid[F[Int]]))

  checkAll(s"NonEmpty$name[Int] with Option",
    NonEmptyTraverseTests[F].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
  checkAll(s"NonEmptyTraverse[NonEmpty$name[A]]", SerializableTests.serializable(NonEmptyTraverse[F]))



  implicit val iso2 = SemigroupalTests.Isomorphisms.invariant[F](Alternative[F])
  checkAll(s"NonEmpty$name[Int]", AlternativeTests[F].alternative[Int, Int, Int])
  checkAll(s"Alternative[NonEmpty$name[Int]]", SerializableTests.serializable(Alternative[F]))


  checkAll(s"NonEmpty$name[Int]", BimonadTests[F].bimonad[Int, Int, Int])
  checkAll("Bimonad[F[A]]", SerializableTests.serializable(Bimonad[F]))

  checkAll(s"NonEmpty$name[Int]", TraverseFilterTests[F].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[F[A]]", SerializableTests.serializable(TraverseFilter[F]))
}