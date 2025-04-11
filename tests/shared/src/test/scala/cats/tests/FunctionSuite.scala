/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats.{
  Applicative,
  Bimonad,
  Contravariant,
  ContravariantMonoidal,
  Defer,
  Distributive,
  Endo,
  Id,
  Monad,
  MonoidK,
  Semigroupal
}
import cats.arrow.{ArrowChoice, Choice, CommutativeArrow}
import cats.kernel.*
import cats.kernel.laws.discipline.{
  BandTests,
  BoundedSemilatticeTests,
  CommutativeGroupTests,
  CommutativeMonoidTests,
  CommutativeSemigroupTests,
  EqTests,
  GroupTests,
  HashTests,
  MonoidTests,
  OrderTests,
  PartialOrderTests,
  SemigroupTests,
  SemilatticeTests,
  SerializableTests
}
import cats.laws.discipline.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import cats.syntax.foldable.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class FunctionSuite extends CatsSuite {

  import Helpers.*

  checkAll("Function0[Int]", SemigroupalTests[Function0].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Function0]", SerializableTests.serializable(Semigroupal[Function0]))

  checkAll("Function0[Int]", DeferTests[Function0].defer[Int])
  checkAll("Defer[Function0[Int]]", SerializableTests.serializable(Defer[Function0]))

  checkAll("Function1[MiniInt, Int]", DeferTests[Function1[MiniInt, *]].defer[Int])
  checkAll("Defer[Function1[Int, Int]]", SerializableTests.serializable(Defer[Function1[Int, *]]))

  checkAll("Function0[Int]", BimonadTests[Function0].bimonad[Int, Int, Int])
  checkAll("Bimonad[Function0]", SerializableTests.serializable(Bimonad[Function0]))

  implicit val iso: Isomorphisms[Function1[MiniInt, *]] = Isomorphisms.invariant[Function1[MiniInt, *]]
  checkAll("Function1[MiniInt, Int]", SemigroupalTests[Function1[MiniInt, *]].semigroupal[Int, Int, Int])

  // TODO: make an binary compatible way to do this
  // checkAll("Function1[Int => *]", DeferTests[Function1[Int, *]].defer[Int])

  property("Defer[Function1[Int, *]].fix computing sum") {
    val sum2 = Defer[Function1[Int, *]].fix[Int] { rec => (n: Int) =>
      if (n <= 0) 0 else n * n + rec(n - 1)
    }

    forAll(Gen.choose(0, 1000)) { n =>
      // don't let n get too large because this consumes stack
      assert(sum2(n) == (0 to n).map { n =>
        n * n
      }.sum)
    }
  }

  checkAll("Semigroupal[Function1[Int, *]]", SerializableTests.serializable(Semigroupal[Function1[Int, *]]))

  checkAll("Function1[MiniInt, Int]", MonadTests[MiniInt => *].monad[Int, Int, Int])
  checkAll("Monad[Int => *]", SerializableTests.serializable(Monad[Int => *]))

  checkAll("Function1[MiniInt, MiniInt]",
           CommutativeArrowTests[Function1].commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, MiniInt]
  )
  checkAll("Arrow[Function1]", SerializableTests.serializable(CommutativeArrow[Function1]))

  checkAll("Function1", ChoiceTests[Function1].choice[MiniInt, Boolean, Int, Long])
  checkAll("Choice[Function1]", SerializableTests.serializable(Choice[Function1]))

  checkAll("Function1", ArrowChoiceTests[Function1].arrowChoice[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, MiniInt])
  checkAll("ArrowChoice[Function1]", SerializableTests.serializable(ArrowChoice[Function1]))

  checkAll("Function1", ContravariantTests[* => MiniInt].contravariant[MiniInt, MiniInt, MiniInt])
  checkAll("Contravariant[* => Int]", SerializableTests.serializable(Contravariant[* => Int]))

  checkAll("Function1", MonoidKTests[λ[α => α => α]].monoidK[MiniInt])
  checkAll("MonoidK[λ[α => α => α]", SerializableTests.serializable(MonoidK[Endo]))

  checkAll("Function1[MiniInt, *]",
           DistributiveTests[MiniInt => *].distributive[Int, Int, Int, Id, Function1[MiniInt, *]]
  )
  checkAll("Distributive[Int => *]", SerializableTests.serializable(Distributive[Int => *]))

  // law checks for the various Function0-related instances
  checkAll("Function0[Eqed]", EqTests[Function0[Eqed]].eqv)
  checkAll("Function0[POrd]", PartialOrderTests[Function0[POrd]].partialOrder)
  checkAll("Function0[Ord]", OrderTests[Function0[Ord]].order)
  checkAll("Function0[Semi]", SemigroupTests[Function0[Semi]].semigroup)
  checkAll("Function0[CSemi]", CommutativeSemigroupTests[Function0[CSemi]].commutativeSemigroup)
  checkAll("Function0[Bnd]", BandTests[Function0[Bnd]].band)
  checkAll("Function0[SL]", SemilatticeTests[Function0[SL]].semilattice)
  checkAll("Function0[BSL]", BoundedSemilatticeTests[Function0[BSL]].boundedSemilattice)
  checkAll("Function0[Mono]", MonoidTests[Function0[Mono]].monoid)
  checkAll("Function0[CMono]", CommutativeMonoidTests[Function0[CMono]].commutativeMonoid)
  checkAll("Function0[Grp]", GroupTests[Function0[Grp]].group)
  checkAll("Function0[CGrp]", CommutativeGroupTests[Function0[CGrp]].commutativeGroup)
  checkAll("Function0[Distributive]", DistributiveTests[Function0].distributive[Int, Int, Int, Id, Function0])
  checkAll("Function0[Hsh]", HashTests[Function0[Hsh]].hash)

  // Test for Arrow applicative
  Applicative[String => *]
  checkAll(
    "Function1[MiniInt, *]",
    ApplicativeTests[Function1[MiniInt, *]](using Applicative.catsApplicativeForArrow[Function1, MiniInt])
      .applicative[Int, Int, Int]
  )

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
  checkAll("Function0", SerializableTests.serializable(Distributive[Function0]))

  // law checks for the various Function1-related instances
  checkAll("Function1[MiniInt, Semi]", SemigroupTests[Function1[MiniInt, Semi]].semigroup)
  checkAll("Function1[MiniInt, CSemi]", CommutativeSemigroupTests[Function1[MiniInt, CSemi]].commutativeSemigroup)
  checkAll("Function1[MiniInt, Bnd]", BandTests[Function1[MiniInt, Bnd]].band)
  checkAll("Function1[MiniInt, SL]", SemilatticeTests[Function1[MiniInt, SL]].semilattice)
  checkAll("Function1[MiniInt, BSL]", BoundedSemilatticeTests[Function1[MiniInt, BSL]].boundedSemilattice)
  checkAll("Function1[MiniInt, Mono]", MonoidTests[Function1[MiniInt, Mono]].monoid)
  checkAll("Function1[MiniInt, CMono]", CommutativeMonoidTests[Function1[MiniInt, CMono]].commutativeMonoid)
  checkAll("Function1[MiniInt, Grp]", GroupTests[Function1[MiniInt, Grp]].group)
  checkAll("Function1[MiniInt, CGrp]", CommutativeGroupTests[Function1[MiniInt, CGrp]].commutativeGroup)
  // Isos for ContravariantMonoidal
  implicit val isoCodomain: Isomorphisms[Function1[*, Long]] = Isomorphisms.invariant[Function1[*, Long]]
  checkAll("Function1[*, Monoid]",
           ContravariantMonoidalTests[Function1[*, Long]].contravariantMonoidal[MiniInt, MiniInt, MiniInt]
  )

  // serialization tests for the various Function1-related instances
  checkAll("Semigroup[String => Semi]", SerializableTests.serializable(Semigroup[String => Semi]))
  checkAll("CommutativeSemigroup[String => Semi]",
           SerializableTests.serializable(CommutativeSemigroup[String => CSemi])
  )
  checkAll("Band[String => Bnd]", SerializableTests.serializable(Band[String => Bnd]))
  checkAll("Semilattice[String => SL]", SerializableTests.serializable(Semilattice[String => SL]))
  checkAll("BoundedSemilattice[String => BSL]", SerializableTests.serializable(BoundedSemilattice[String => BSL]))
  checkAll("Monoid[String => Mono]", SerializableTests.serializable(Monoid[String => Mono]))
  checkAll("CommutativeMonoid[String => CMono]", SerializableTests.serializable(CommutativeMonoid[String => CMono]))
  checkAll("Group[String => Grp]", SerializableTests.serializable(Group[String => Grp]))
  checkAll("CommutativeGroup[String => CGrp]", SerializableTests.serializable(CommutativeGroup[String => CGrp]))
  checkAll("ContravariantMonoidal[Function1[*, Monoid]]",
           SerializableTests.serializable(ContravariantMonoidal[* => Long])
  )

  test("MonoidK[Endo] is stack safe on combineK") {
    def incrementAll(as: Int): Int = as + 1
    val bigList: List[Int => Int] = List.fill(50000)(incrementAll)

    val sumAll = bigList.combineAll(MonoidK[Endo].algebra)
    List(1, 1, 1).map(sumAll)
  }

  property("Semigroup[Function0[Semi]] is stack safe on combine") {
    forAll { (f: Function0[Semi]) =>
      1.to(50000).foldLeft(f)((acc, _) => Semigroup[Function0[Semi]].combine(acc, f)).apply()
      true
    }
  }

  property("Semigroup[Function1[MiniInt, Semi]] is stack safe on combine") {
    forAll { (i: MiniInt, f: Function1[MiniInt, Semi]) =>
      1.to(50000).foldLeft(f)((acc, _) => Semigroup[Function1[MiniInt, Semi]].combine(acc, f)).apply(i)
      true
    }
  }
}
