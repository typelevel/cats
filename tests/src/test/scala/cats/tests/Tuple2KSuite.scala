package cats
package tests

import cats.data.{Const, Tuple2K, Validated}
import cats.Contravariant
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.kernel.laws.discipline.{EqTests, OrderTests, PartialOrderTests}

class Tuple2KSuite extends CatsSuite {
  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Tuple2K[Option, List, ?]]
  checkAll("Tuple2K[Eval, Eval, ?]", DeferTests[Tuple2K[Eval, Eval, ?]].defer[Int])
  checkAll("Tuple2K[Option, List, Int]", SemigroupalTests[λ[α => Tuple2K[Option, List, α]]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Tuple2K[Option, List, Int]]",
           SerializableTests.serializable(Semigroupal[λ[α => Tuple2K[Option, List, α]]]))

  checkAll("Tuple2K[Option, List, Int]", AlternativeTests[λ[α => Tuple2K[Option, List, α]]].alternative[Int, Int, Int])
  checkAll("Alternative[Tuple2K[Option, List, Int]]",
           SerializableTests.serializable(Alternative[λ[α => Tuple2K[Option, List, α]]]))

  checkAll("Tuple2K[Show, Order, Int]",
           ContravariantTests[λ[α => Tuple2K[Show, Order, α]]].contravariant[Int, Int, Int])
  checkAll("Contravariant[Tuple2K[Show, Order, Int]]",
           SerializableTests.serializable(Contravariant[λ[α => Tuple2K[Show, Order, α]]]))

  checkAll(
    "Tuple2K[Const[String, ?], Const[Int, ?], Int]",
    ContravariantMonoidalTests[λ[α => Tuple2K[Const[String, ?], Const[Int, ?], α]]].contravariantMonoidal[Int, Int, Int]
  )
  checkAll(
    "ContravariantMonoidal[Tuple2K[Const[String, ?], Const[Int, ?], Int]]",
    SerializableTests.serializable(ContravariantMonoidal[λ[α => Tuple2K[Const[String, ?], Const[Int, ?], α]]])
  )

  // Coproduct isos for DecideableTests
  implicit val coproductIso = DecideableTests.Isomorphisms.invariant[Tuple2K[Const[String, ?], Const[Int, ?], ?]]

  checkAll("Tuple2K[Const[String, ?], Const[Int, ?], Long]",
           DecideableTests[λ[α => Tuple2K[Const[String, ?], Const[Int, ?], α]]].decideable[Long, Long, Long])
  checkAll(
    "Decideable[Tuple2K[Const[String, ?], Const[Int, ?], Long]]",
    SerializableTests.serializable(Decideable[λ[α => Tuple2K[Const[String, ?], Const[Int, ?], α]]])
  )

  checkAll("Show[Tuple2K[Option, Option, Int]]", SerializableTests.serializable(Show[Tuple2K[Option, Option, Int]]))

  {
    implicit val monoidK = ListWrapper.monoidK
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]", MonoidKTests[Tuple2K[ListWrapper, ListWrapper, ?]].monoidK[Int])
    checkAll("MonoidK[Tuple2K[ListWrapper, ListWrapper, ?]]",
             SerializableTests.serializable(MonoidK[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val semigroupK = ListWrapper.semigroupK
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]",
             SemigroupKTests[Tuple2K[ListWrapper, ListWrapper, ?]].semigroupK[Int])
    checkAll("SemigroupK[Tuple2K[ListWrapper, ListWrapper, ?]]",
             SerializableTests.serializable(SemigroupK[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val apply = ListWrapper.applyInstance
    implicit val iso = SemigroupalTests.Isomorphisms.invariant[Tuple2K[ListWrapper, ListWrapper, ?]]
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]",
             ApplyTests[Tuple2K[ListWrapper, ListWrapper, ?]].apply[Int, Int, Int])
    checkAll("Apply[Tuple2K[ListWrapper, ListWrapper, ?]]",
             SerializableTests.serializable(Apply[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val optionApply = Apply[Option]
    implicit val validatedApply = Apply[Validated[Int, ?]]
    checkAll("Tuple2K[Option, Validated[Int, ?], ?]",
             CommutativeApplyTests[Tuple2K[Option, Validated[Int, ?], ?]].commutativeApply[Int, Int, Int])
    checkAll("Apply[Tuple2K[Option, Validated[Int, ?], ?]]",
             SerializableTests.serializable(CommutativeApply[Tuple2K[Option, Validated[Int, ?], ?]]))
  }

  {
    checkAll("Tuple2K[Option, Validated[Int, ?], ?]",
             CommutativeApplicativeTests[Tuple2K[Option, Validated[Int, ?], ?]].commutativeApplicative[Int, Int, Int])
    checkAll("Applicative[Tuple2K[Option, Validated[Int, ?], ?]]",
             SerializableTests.serializable(CommutativeApplicative[Tuple2K[Option, Validated[Int, ?], ?]]))
  }

  {
    implicit val functor = ListWrapper.functor
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]",
             FunctorTests[Tuple2K[ListWrapper, ListWrapper, ?]].functor[Int, Int, Int])
    checkAll("Functor[Tuple2K[ListWrapper, ListWrapper, ?]]",
             SerializableTests.serializable(Functor[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val monad = ListWrapper.monad
    implicit val iso = SemigroupalTests.Isomorphisms.invariant[Tuple2K[ListWrapper, ListWrapper, ?]]
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]",
             MonadTests[Tuple2K[ListWrapper, ListWrapper, ?]].monad[Int, Int, Int])
    checkAll("Monad[Tuple2K[ListWrapper, ListWrapper, ?]]",
             SerializableTests.serializable(Monad[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val foldable = ListWrapper.foldable
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]",
             FoldableTests[Tuple2K[ListWrapper, ListWrapper, ?]].foldable[Int, Int])
    checkAll("Foldable[Tuple2K[ListWrapper, ListWrapper, ?]]",
             SerializableTests.serializable(Foldable[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val traverse = ListWrapper.traverse
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]",
             TraverseTests[Tuple2K[ListWrapper, ListWrapper, ?]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[Tuple2K[ListWrapper, ListWrapper, ?]]",
             SerializableTests.serializable(Traverse[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val alternative = ListWrapper.alternative
    implicit val iso = SemigroupalTests.Isomorphisms.invariant[Tuple2K[ListWrapper, ListWrapper, ?]]
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]",
             AlternativeTests[Tuple2K[ListWrapper, ListWrapper, ?]].alternative[Int, Int, Int])
    checkAll("Alternative[Tuple2K[ListWrapper, ListWrapper, ?]]",
             SerializableTests.serializable(Alternative[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val E = ListWrapper.eqv[Int]
    implicit val O = ListWrapper.order[Int]
    implicit val P = ListWrapper.partialOrder[Int]

    checkAll("Tuple2K[ListWrapper, ListWrapper, Int]", EqTests[Tuple2K[ListWrapper, ListWrapper, Int]].eqv)
    checkAll("Tuple2K[ListWrapper, ListWrapper, Int]", OrderTests[Tuple2K[ListWrapper, ListWrapper, Int]].order)
    checkAll("Tuple2K[ListWrapper, ListWrapper, Int]",
             PartialOrderTests[Tuple2K[ListWrapper, ListWrapper, Int]].partialOrder)
  }

  {
    checkAll("Tuple2K[Function0, Function0, ?]",
             DistributiveTests[Tuple2K[Function0, Function0, ?]].distributive[Int, Int, Int, Option, Function0])
    checkAll("Distributive[Tuple2K[Function0, Function0, ?]]",
             SerializableTests.serializable(Distributive[Tuple2K[Function0, Function0, ?]]))
  }

  test("show") {
    forAll { (l1: Option[Int], l2: Option[Int]) =>
      val tuple = Tuple2K(l1, l2)

      Show[Tuple2K[Option, Option, Int]].show(tuple) should ===(
        s"Tuple2K(${Show[Option[Int]].show(l1)}, ${Show[Option[Int]].show(l2)})"
      )
    }
  }

  test("double swap is identity") {
    forAll { (l1: Option[String], l2: List[String]) =>
      val tuple = Tuple2K(l1, l2)

      tuple.swap.swap should ===(tuple)
    }
  }

}
