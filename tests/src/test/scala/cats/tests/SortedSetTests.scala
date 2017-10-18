package cats
package tests

import cats.kernel.{BoundedSemilattice, Semilattice}
import cats.laws.discipline.{FoldableTests, SemigroupKTests, SerializableTests}
import cats.kernel.laws.discipline.{BoundedSemilatticeTests, HashTests => HashLawTests, MonoidTests => MonoidLawTests, PartialOrderTests => PartialOrderLawTests}
import cats.laws.discipline.arbitrary._

import scala.collection.immutable.SortedSet

class SortedSetTests extends CatsSuite {
  checkAll("SortedSet[Int]", MonoidLawTests[SortedSet[Int]].monoid)

  checkAll("SortedSet[Int]", SemigroupKTests[SortedSet].semigroupK[Int])
  checkAll("SemigroupK[SortedSet]", SerializableTests.serializable(SemigroupK[SortedSet]))

  checkAll("SortedSet[Int]", FoldableTests[SortedSet].foldable[Int, Int])
  checkAll("PartialOrder[SortedSet[Int]]", PartialOrderLawTests[SortedSet[Int]].partialOrder)
  checkAll("PartialOrder[SortedSet[Int]].reverse", PartialOrderLawTests(PartialOrder[SortedSet[Int]].reverse).partialOrder)
  checkAll("PartialOrder[SortedSet[Int]].reverse.reverse", PartialOrderLawTests(PartialOrder[SortedSet[Int]].reverse.reverse).partialOrder)

  checkAll("BoundedSemilattice[SortedSet[Int]]", BoundedSemilatticeTests[SortedSet[Int]].boundedSemilattice)
  checkAll("BoundedSemilattice[SortedSet[Int]]", SerializableTests.serializable(BoundedSemilattice[SortedSet[Int]]))

  checkAll("Semilattice.asMeetPartialOrder[SortedSet[Int]]", PartialOrderLawTests(Semilattice.asMeetPartialOrder[SortedSet[Int]]).partialOrder)
  checkAll("Semilattice.asJoinPartialOrder[SortedSet[Int]]", PartialOrderLawTests(Semilattice.asJoinPartialOrder[SortedSet[Int]]).partialOrder)
  checkAll("Hash[SortedSet[Int]]" , HashLawTests[SortedSet[Int]].hash)


  test("show keeps separate entries for items that map to identical strings"){
    //note: this val name has to be the same to shadow the cats.instances instance
    implicit val catsStdShowForInt: Show[Int] = Show.show(_ => "1")
    // an implementation implemented as set.map(_.show).mkString(", ") would
    // only show one entry in the result instead of 3, because SortedSet.map combines
    // duplicate items in the codomain.
    SortedSet(1, 2, 3).show should === ("SortedSet(1, 1, 1)")
  }
}