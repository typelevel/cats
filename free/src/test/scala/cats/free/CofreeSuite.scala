package cats
package free

import cats.data.{NonEmptyList, OptionT}
import cats.laws.discipline.{SemigroupalTests, ComonadTests, ReducibleTests, SerializableTests, TraverseTests}
import cats.syntax.list._
import cats.tests.{CatsSuite, Spooky}
import org.scalacheck.{Arbitrary, Cogen, Gen}

class CofreeSuite extends CatsSuite {

  import CofreeSuite._

  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Cofree[Option, ?]]

  checkAll("Cofree[Option, ?]", ComonadTests[Cofree[Option, ?]].comonad[Int, Int, Int])
  locally {
    implicit val instance = Cofree.catsTraverseForCofree[Option]
    checkAll("Cofree[Option, ?]", TraverseTests[Cofree[Option, ?]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[Cofree[Option, ?]]", SerializableTests.serializable(Traverse[Cofree[Option, ?]]))
  }
  locally {
    implicit val instance = Cofree.catsReducibleForCofree[Option]
    checkAll("Cofree[Option, ?]", ReducibleTests[Cofree[Option, ?]].reducible[Option, Int, Int])
    checkAll("Reducible[Cofree[Option, ?]]", SerializableTests.serializable(Reducible[Cofree[Option, ?]]))
  }
  checkAll("Comonad[Cofree[Option, ?]]", SerializableTests.serializable(Comonad[Cofree[Option, ?]]))

  test("Cofree.unfold") {
    val unfoldedHundred: CofreeNel[Int] = Cofree.unfold[Option, Int](0)(i => if (i == 100) None else Some(i + 1))
    val nelUnfoldedHundred: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(List.tabulate(101)(identity))
    cofNelToNel(unfoldedHundred) should ===(nelUnfoldedHundred)
  }

  test("Cofree.ana") {
    val unfoldedHundred: CofreeNel[String] = Cofree.ana[Option, Int, String](0)(i => if (i == 100) None else Some(i + 1), _.toString)
    val nelUnfoldedHundred: NonEmptyList[String] = NonEmptyList.fromListUnsafe(List.tabulate(101)(_.toString))
    cofNelToNel(unfoldedHundred) should ===(nelUnfoldedHundred)
  }

  test("Cofree.tailForced") {
    val spooky = new Spooky
    val incrementor =
      Cofree.unfold[Id, Int](spooky.counter) { _ => spooky.increment(); spooky.counter }
    spooky.counter should ===(0)
    incrementor.tailForced
    spooky.counter should ===(1)
  }

  test("Cofree.forceTail") {
    val spooky = new Spooky
    val incrementor =
      Cofree.unfold[Id, Int](spooky.counter) { _ => spooky.increment(); spooky.counter }
    spooky.counter should ===(0)
    incrementor.forceTail
    spooky.counter should ===(1)
  }

  test("Cofree.forceAll") {
    val spooky = new Spooky
    val incrementor =
      Cofree.unfold[Option, Int](spooky.counter)(i =>
        if (i == 5) {
          None
        } else {
          spooky.increment()
          Some(spooky.counter)
        })
    spooky.counter should ===(0)
    incrementor.forceAll
    spooky.counter should ===(5)
  }

  test("Cofree.mapBranchingRoot") {
    val unfoldedHundred: CofreeNel[Int] = Cofree.unfold[Option, Int](0)(i => if (i == 100) None else Some(i + 1))
    val withNoneRoot = unfoldedHundred.mapBranchingRoot(λ[Option ~> Option](_ => None))
    val nelUnfoldedOne: NonEmptyList[Int] = NonEmptyList.one(0)
    cofNelToNel(withNoneRoot) should ===(nelUnfoldedOne)
  }

  val unfoldedHundred: Cofree[Option, Int] = Cofree.unfold[Option, Int](0)(i => if (i == 100) None else Some(i + 1))
  test("Cofree.mapBranchingS/T") {
    val toList = λ[Option ~> List](_.toList)
    val toNelS = unfoldedHundred.mapBranchingS(toList)
    val toNelT = unfoldedHundred.mapBranchingT(toList)
    val nelUnfoldedOne: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(List.tabulate(101)(identity))
    cofRoseTreeToNel(toNelS) should ===(nelUnfoldedOne)
    cofRoseTreeToNel(toNelT) should ===(nelUnfoldedOne)
  }

  val nelUnfoldedHundred: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(List.tabulate(101)(identity))

  test("Cofree.cata") {
    val cata =
      Cofree.cata[Option, Int, NonEmptyList[Int]](unfoldedHundred)(
        (i, lb) => Eval.now(NonEmptyList(i, lb.fold[List[Int]](Nil)(_.toList)))
      ).value
    cata should ===(nelUnfoldedHundred)
  }

  test("Cofree.cataM") {

    type EvalOption[A] = OptionT[Eval, A]

    val folder: (Int, Option[NonEmptyList[Int]]) => EvalOption[NonEmptyList[Int]] =
      (i, lb) => if (i > 100) OptionT.none else OptionT.some(NonEmptyList(i, lb.fold[List[Int]](Nil)(_.toList)))
    val inclusion = OptionT.liftK[Eval]

    val cataHundred =
      Cofree.cataM[Option, EvalOption, Int, NonEmptyList[Int]](unfoldedHundred)(folder)(inclusion).value.value
    val cataHundredOne =
      Cofree.cataM[Option, EvalOption, Int, NonEmptyList[Int]](
        Cofree[Option, Int](101, Eval.now(Some(unfoldedHundred)))
      )(folder)(inclusion).value.value
    cataHundred should ===(Some(nelUnfoldedHundred))
    cataHundredOne should ===(None)
  }

}

object CofreeSuite extends CofreeSuiteInstances

sealed trait CofreeSuiteInstances {

  type CofreeNel[A] = Cofree[Option, A]
  type CofreeRoseTree[A] = Cofree[List, A]

  implicit def cofNelEq[A](implicit e: Eq[A]): Eq[CofreeNel[A]] = new Eq[CofreeNel[A]] {
    override def eqv(a: CofreeNel[A], b: CofreeNel[A]): Boolean = {
      def tr(a: CofreeNel[A], b: CofreeNel[A]): Boolean =
        (a.tailForced, b.tailForced) match {
          case (Some(at), Some(bt)) if e.eqv(a.head, b.head) => tr(at, bt)
          case (None, None) if e.eqv(a.head, b.head) => true
          case _ => false
        }
      tr(a, b)
    }
  }


  implicit def CofreeOptionCogen[A: Cogen]: Cogen[CofreeNel[A]] =
    implicitly[Cogen[List[A]]].contramap[CofreeNel[A]](cofNelToNel(_).toList)

  implicit def CofreeOptionArb[A: Arbitrary]: Arbitrary[CofreeNel[A]] = {
    val arb = Arbitrary {
      Gen.resize(20, Gen.nonEmptyListOf(implicitly[Arbitrary[A]].arbitrary))
    }
    Arbitrary {
      arb.arbitrary.map(l => (l.head, l.tail) match {
        case (h, Nil) => nelToCofNel(NonEmptyList(h, Nil))
        case (h, t) => nelToCofNel(NonEmptyList(h, t))
      })
    }
  }

  val nelToCofNel = λ[NonEmptyList ~> CofreeNel](fa =>
    Cofree(fa.head, Eval.later(fa.tail.toNel.map(apply))))

  val cofNelToNel = λ[CofreeNel ~> NonEmptyList](fa =>
    NonEmptyList(fa.head, fa.tailForced.map(apply(_).toList).getOrElse(Nil)))

  val cofRoseTreeToNel = λ[CofreeRoseTree ~> NonEmptyList](fa =>
      NonEmptyList(fa.head, fa.tailForced.flatMap(apply(_).toList)))
}
