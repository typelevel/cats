package cats
package tests

import cats.Invariant
import cats.kernel._
import cats.kernel.laws.discipline.{SemigroupTests, MonoidTests, GroupTests, _}
import cats.laws.discipline.{InvariantMonoidalTests, InvariantTests, SerializableTests, InvariantSemigroupalTests}
import cats.laws.discipline.eq._
import org.scalacheck.{Arbitrary, Gen}

class AlgebraInvariantSuite extends CatsSuite {

  val intMultiplication: CommutativeMonoid[Int] = new CommutativeMonoid[Int] {
    val empty = 1
    def combine(x: Int, y: Int): Int = x * y
  }

  val maxInt: Monoid[Int] = new Monoid[Int] {
    val empty = Int.MinValue
    def combine(x: Int, y: Int): Int = if (x > y) x else y
  }

  val genMonoidInt: Gen[Monoid[Int]] =
    Gen.oneOf(implicitly[Monoid[Int]], intMultiplication, maxInt)

  val genCommutativeMonoidInt: Gen[CommutativeMonoid[Int]] =
    Gen.oneOf(implicitly[CommutativeMonoid[Int]], intMultiplication)

  val genBoundedSemilatticeSetInt: Gen[BoundedSemilattice[Set[Int]]] =
    Gen.const(implicitly[BoundedSemilattice[Set[Int]]])

  val genCommutativeGroupInt: Gen[CommutativeGroup[Int]] =
    Gen.const(implicitly[CommutativeGroup[Int]])

  implicit val arbMonoidInt: Arbitrary[Monoid[Int]] =
    Arbitrary(genMonoidInt)

  implicit val arbSemigroupInt: Arbitrary[Semigroup[Int]] =
    Arbitrary(genMonoidInt)

  implicit val arbCommutativeMonoidInt: Arbitrary[CommutativeMonoid[Int]] =
    Arbitrary(genCommutativeMonoidInt)

  implicit val arbCommutativeSemigroupInt: Arbitrary[CommutativeSemigroup[Int]] =
    Arbitrary(genCommutativeMonoidInt)

  implicit val arbBandSetInt: Arbitrary[Band[Set[Int]]] =
    Arbitrary(genBoundedSemilatticeSetInt)

  implicit val arbSemilatticeSetInt: Arbitrary[Semilattice[Set[Int]]] =
    Arbitrary(genBoundedSemilatticeSetInt)

  implicit val arbBoundedSemilatticeSetInt: Arbitrary[BoundedSemilattice[Set[Int]]] =
    Arbitrary(genBoundedSemilatticeSetInt)

  implicit val arbGroupInt: Arbitrary[Group[Int]] =
    Arbitrary(genCommutativeGroupInt)

  implicit val arbCommutativeGroupInt: Arbitrary[CommutativeGroup[Int]] =
    Arbitrary(genCommutativeGroupInt)



  checkAll("InvariantMonoidal[Semigroup]", SemigroupTests[Int](InvariantMonoidal[Semigroup].pure(0)).semigroup)
  checkAll("InvariantMonoidal[CommutativeSemigroup]", CommutativeSemigroupTests[Int](InvariantMonoidal[CommutativeSemigroup].pure(0)).commutativeSemigroup)

  checkAll("InvariantSemigroupal[Monoid]", InvariantSemigroupalTests[Monoid].invariantSemigroupal[Int, Int, Int])


  {
    val S: Semigroup[Int] = Semigroup[Int].imap(identity)(identity)
    checkAll("Semigroup[Int]", SemigroupTests[Int](S).semigroup)
  }

  {
    val S: Monoid[Int] = Monoid[Int].imap(identity)(identity)
    checkAll("Monoid[Int]", MonoidTests[Int](S).monoid)
  }

  {
    val S: Group[Int] = Group[Int].imap(identity)(identity)
    checkAll("Group[Int]", GroupTests[Int](S).group)
  }

  {
    val S: CommutativeSemigroup[Int] = CommutativeSemigroup[Int].imap(identity)(identity)
    checkAll("CommutativeSemigroup[Int]", CommutativeSemigroupTests[Int](S).commutativeSemigroup)
  }

  {
    val S: CommutativeMonoid[Int] = CommutativeMonoid[Int].imap(identity)(identity)
    checkAll("CommutativeMonoid[Int]", CommutativeMonoidTests[Int](S).commutativeMonoid)
  }


  {
    val S: CommutativeGroup[Int] = CommutativeGroup[Int].imap(identity)(identity)
    checkAll("CommutativeGroup[Int]", CommutativeGroupTests[Int](S).commutativeGroup)
  }


  {
    val S: Band[Set[Int]] = Band[Set[Int]].imap(identity)(identity)
    checkAll("Band[Set[Int]]", BandTests[Set[Int]](S).band)
  }

  {
    val S: Semilattice[Set[Int]] = Semilattice[Set[Int]].imap(identity)(identity)
    checkAll("Semilattice[Set[Int]]", SemilatticeTests[Set[Int]](S).semilattice)
  }

  {
    val S: BoundedSemilattice[Set[Int]] = BoundedSemilattice[Set[Int]].imap(identity)(identity)
    checkAll("BoundedSemilattice[Set[Int]]", BoundedSemilatticeTests[Set[Int]](S).boundedSemilattice)
  }


  checkAll("Invariant[Semigroup]", InvariantTests[Semigroup].invariant[Int, Int, Int])
  checkAll("Invariant[Semigroup]", SerializableTests.serializable(Invariant[Semigroup]))

  checkAll("Invariant[CommutativeSemigroup]", InvariantTests[CommutativeSemigroup].invariant[Int, Int, Int])
  checkAll("Invariant[CommutativeSemigroup]", SerializableTests.serializable(Invariant[CommutativeSemigroup]))

  checkAll("Invariant[Band]", InvariantTests[Band].invariant[Set[Int], Set[Int], Set[Int]])
  checkAll("Invariant[Band]", SerializableTests.serializable(Invariant[Band]))

  checkAll("Invariant[Monoid]", InvariantTests[Monoid].invariant[Int, Int, Int])
  checkAll("Invariant[Monoid]", SerializableTests.serializable(Invariant[Monoid]))

  checkAll("Invariant[Semilattice]", InvariantTests[Semilattice].invariant[Set[Int], Set[Int], Set[Int]])
  checkAll("Invariant[Semilattice]", SerializableTests.serializable(Invariant[Semilattice]))

  checkAll("Invariant[CommutativeMonoid]", InvariantTests[CommutativeMonoid].invariant[Int, Int, Int])
  checkAll("Invariant[CommutativeMonoid]", SerializableTests.serializable(Invariant[CommutativeMonoid]))

  checkAll("Invariant[BoundedSemilattice]", InvariantTests[BoundedSemilattice].invariant[Set[Int], Set[Int], Set[Int]])
  checkAll("Invariant[BoundedSemilattice]", SerializableTests.serializable(Invariant[BoundedSemilattice]))

  checkAll("Invariant[Group]", InvariantTests[Group].invariant[Int, Int, Int])
  checkAll("Invariant[Group]", SerializableTests.serializable(Invariant[Group]))

  checkAll("Invariant[CommutativeGroup]", InvariantTests[CommutativeGroup].invariant[Int, Int, Int])
  checkAll("Invariant[CommutativeGroup]", SerializableTests.serializable(Invariant[CommutativeGroup]))

  checkAll("InvariantMonoidal[Semigroup]", InvariantMonoidalTests[Semigroup].invariantMonoidal[Int, Int, Int])
  checkAll("InvariantMonoidal[Semigroup]", SerializableTests.serializable(InvariantMonoidal[Semigroup]))

  checkAll("InvariantMonoidal[CommutativeSemigroup]", InvariantMonoidalTests[CommutativeSemigroup].invariantMonoidal[Int, Int, Int])
  checkAll("InvariantMonoidal[CommutativeSemigroup]", SerializableTests.serializable(InvariantMonoidal[CommutativeSemigroup]))

}
