package cats
package tests

import cats.arrow.{Choice, CommutativeArrow}
import cats.kernel.laws.discipline.{SerializableTests, _}
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import cats.kernel.{ CommutativeSemigroup, CommutativeMonoid, CommutativeGroup }
import cats.kernel.{ Band, Semilattice, BoundedSemilattice }


class FunctionTests extends CatsSuite {

  import Helpers._

  checkAll("Function0[Int]", CartesianTests[Function0].cartesian[Int, Int, Int])
  checkAll("Cartesian[Function0]", SerializableTests.serializable(Cartesian[Function0]))

  checkAll("Function0[Int]", BimonadTests[Function0].bimonad[Int, Int, Int])
  checkAll("Bimonad[Function0]", SerializableTests.serializable(Bimonad[Function0]))

  implicit val iso = CartesianTests.Isomorphisms.invariant[Function1[Int, ?]]
  checkAll("Function1[Int, Int]", CartesianTests[Function1[Int, ?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Function1[Int, ?]]", SerializableTests.serializable(Cartesian[Function1[Int, ?]]))

  checkAll("Function1[Int, Int]", MonadTests[Int => ?].monad[Int, Int, Int])
  checkAll("Monad[Int => ?]", SerializableTests.serializable(Monad[Int => ?]))

  checkAll("Function1[Int, Int]", CommutativeArrowTests[Function1].commutativeArrow[Int, Int, Int, Int, Int, Int])
  checkAll("Arrow[Function1]", SerializableTests.serializable(CommutativeArrow[Function1]))

  checkAll("Function1[Int, Int]", ChoiceTests[Function1].choice[Int, Int, Int, Int])
  checkAll("Choice[Function1]", SerializableTests.serializable(Choice[Function1]))

  checkAll("Function1[Int, Int]", ContravariantTests[? => Int].contravariant[Int, Int, Int])
  checkAll("Contravariant[? => Int]", SerializableTests.serializable(Contravariant[? => Int]))

  checkAll("Function1[Int, Int]", MonoidKTests[λ[α => α => α]].monoidK[Int])
  checkAll("MonoidK[λ[α => α => α]", SerializableTests.serializable(catsStdMonoidKForFunction1))


  // law checks for the various Function0-related instances
  checkAll("Function0[Eqed]", EqLawTests[Function0[Eqed]].eqv)
  checkAll("Function0[POrd]", PartialOrderLawTests[Function0[POrd]].partialOrder)
  checkAll("Function0[Ord]", OrderLawTests[Function0[Ord]].order)
  checkAll("Function0[Semi]", SemigroupLawTests[Function0[Semi]].semigroup)
  checkAll("Function0[CSemi]", CommutativeSemigroupTests[Function0[CSemi]].commutativeSemigroup)
  checkAll("Function0[Bnd]", BandTests[Function0[Bnd]].band)
  checkAll("Function0[SL]", SemilatticeTests[Function0[SL]].semilattice)
  checkAll("Function0[BSL]", BoundedSemilatticeTests[Function0[BSL]].boundedSemilattice)
  checkAll("Function0[Mono]", MonoidLawTests[Function0[Mono]].monoid)
  checkAll("Function0[CMono]", CommutativeMonoidTests[Function0[CMono]].commutativeMonoid)
  checkAll("Function0[Grp]", GroupLawTests[Function0[Grp]].group)
  checkAll("Function0[CGrp]", CommutativeGroupTests[Function0[CGrp]].commutativeGroup)

  // serialization tests for the various Function0-related instances
  checkAll("Eq[() => Eqed]", SerializableTests.serializable(Eq[() => Eqed]))
  checkAll("PartialOrder[() => POrd]", SerializableTests.serializable(PartialOrder[() => POrd]))
  checkAll("Order[() => Ord]", SerializableTests.serializable(Order[() => Ord]))
  checkAll("Semigroup[() => Semi]", SerializableTests.serializable(Semigroup[() => Semi]))
  checkAll("CommutativeSemigroup[() => Semi]", SerializableTests.serializable(CommutativeSemigroup[() => CSemi]))
  checkAll("Band[() => Bnd]", SerializableTests.serializable(Band[() => Bnd]))
  checkAll("Semilattice[() => SL]", SerializableTests.serializable(Semilattice[() => SL]))
  checkAll("BoundedSemilattice[() => BSL]", SerializableTests.serializable(BoundedSemilattice[() => BSL]))
  checkAll("Monoid[() => Mono]", SerializableTests.serializable(Monoid[() => Mono]))
  checkAll("CommutativeMonoid[() => CMono]", SerializableTests.serializable(CommutativeMonoid[() => CMono]))
  checkAll("Group[() => Grp]", SerializableTests.serializable(Group[() => Grp]))
  checkAll("CommutativeGroup[() => CGrp]", SerializableTests.serializable(CommutativeGroup[() => CGrp]))


  // law checks for the various Function1-related instances
  checkAll("Function1[String, Semi]", SemigroupLawTests[Function1[String, Semi]].semigroup)
  checkAll("Function1[String, CSemi]", CommutativeSemigroupTests[Function1[String, CSemi]].commutativeSemigroup)
  checkAll("Function1[String, Bnd]", BandTests[Function1[String, Bnd]].band)
  checkAll("Function1[String, SL]", SemilatticeTests[Function1[String, SL]].semilattice)
  checkAll("Function1[String, BSL]", BoundedSemilatticeTests[Function1[String, BSL]].boundedSemilattice)
  checkAll("Function1[String, Mono]", MonoidLawTests[Function1[String, Mono]].monoid)
  checkAll("Function1[String, CMono]", CommutativeMonoidTests[Function1[String, CMono]].commutativeMonoid)
  checkAll("Function1[String, Grp]", GroupLawTests[Function1[String, Grp]].group)
  checkAll("Function1[String, CGrp]", CommutativeGroupTests[Function1[String, CGrp]].commutativeGroup)

  // serialization tests for the various Function1-related instances
  checkAll("Semigroup[String => Semi]", SerializableTests.serializable(Semigroup[String => Semi]))
  checkAll("CommutativeSemigroup[String => Semi]", SerializableTests.serializable(CommutativeSemigroup[String => CSemi]))
  checkAll("Band[String => Bnd]", SerializableTests.serializable(Band[String => Bnd]))
  checkAll("Semilattice[String => SL]", SerializableTests.serializable(Semilattice[String => SL]))
  checkAll("BoundedSemilattice[String => BSL]", SerializableTests.serializable(BoundedSemilattice[String => BSL]))
  checkAll("Monoid[String => Mono]", SerializableTests.serializable(Monoid[String => Mono]))
  checkAll("CommutativeMonoid[String => CMono]", SerializableTests.serializable(CommutativeMonoid[String => CMono]))
  checkAll("Group[String => Grp]", SerializableTests.serializable(Group[String => Grp]))
  checkAll("CommutativeGroup[String => CGrp]", SerializableTests.serializable(CommutativeGroup[String => CGrp]))
}
