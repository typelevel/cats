package cats.tests

import cats.data.Const
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.kernel.{Monoid, Semigroup}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{DecidableTests, MiniInt}
import cats.{Contravariant, ContravariantMonoidal, ContravariantSemigroupal}

class ContravariantSuite extends CatsSuite {

  test("narrow equals contramap(identity)") {
    implicit val constInst: Contravariant[Const[Int, *]] = Const.catsDataContravariantForConst[Int]
    forAll { (i: Int) =>
      val const: Const[Int, Option[Int]] = Const[Int, Option[Int]](i)
      val narrowed: Const[Int, Some[Int]] = constInst.narrow[Option[Int], Some[Int]](const)
      narrowed should ===(constInst.contramap(const)(identity[Option[Int]](_: Some[Int])))
      assert(narrowed eq const)
    }
  }

  checkAll(
    "Decidable[Predicate]",
    DecidableTests[Predicate].decidable[Boolean, Boolean, Boolean]
  )

  {
    implicit val predicateMonoid: Monoid[Predicate[MiniInt]] = ContravariantMonoidal.monoid[Predicate, MiniInt]
    checkAll("ContravariantMonoidal[Predicate].monoid", MonoidTests[Predicate[MiniInt]].monoid)
  }
  {
    implicit val predicateSemigroup: Semigroup[Predicate[MiniInt]] =
      ContravariantSemigroupal.semigroup[Predicate, MiniInt]
    checkAll("ContravariantSemigroupal[Predicate].semigroup", SemigroupTests[Predicate[MiniInt]].semigroup)
  }

}
