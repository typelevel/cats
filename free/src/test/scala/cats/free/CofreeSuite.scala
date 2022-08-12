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

package cats.free

import cats.{~>, Comonad, Eval, Id, Reducible, Traverse}
import cats.data.{NonEmptyList, OptionT}
import cats.instances.all._
import cats.kernel.Eq
import cats.laws.discipline.{ComonadTests, ReducibleTests, SerializableTests, TraverseTests}
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.list._
import cats.tests.{CatsSuite, Spooky}
import org.scalacheck.{Arbitrary, Cogen, Gen}
import cats.syntax.eq._

import scala.annotation.tailrec

class CofreeSuite extends CatsSuite {

  import CofreeSuite._

  implicit val iso: Isomorphisms[Cofree[Option, *]] = Isomorphisms.invariant[Cofree[Option, *]]

  checkAll("Cofree[Option, *]", ComonadTests[Cofree[Option, *]].comonad[Int, Int, Int])
  locally {
    implicit val instance: Traverse[Cofree[Option, *]] = Cofree.catsTraverseForCofree[Option]
    checkAll("Cofree[Option, *]", TraverseTests[Cofree[Option, *]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[Cofree[Option, *]]", SerializableTests.serializable(Traverse[Cofree[Option, *]]))
  }
  locally {
    implicit val instance: Reducible[Cofree[Option, *]] = Cofree.catsReducibleForCofree[Option]
    checkAll("Cofree[Option, *]", ReducibleTests[Cofree[Option, *]].reducible[Option, Int, Int])
    checkAll("Reducible[Cofree[Option, *]]", SerializableTests.serializable(Reducible[Cofree[Option, *]]))
  }
  checkAll("Comonad[Cofree[Option, *]]", SerializableTests.serializable(Comonad[Cofree[Option, *]]))

  test("Cofree.unfold") {
    val unfoldedHundred: CofreeNel[Int] = Cofree.unfold[Option, Int](0)(i => if (i == 100) None else Some(i + 1))
    val nelUnfoldedHundred: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(List.tabulate(101)(identity))
    assert(cofNelToNel(unfoldedHundred) === nelUnfoldedHundred)
  }

  test("Cofree.ana") {
    val anaHundred: CofreeNel[Int] =
      Cofree.ana[Option, List[Int], Int](List.tabulate(101)(identity))(l => if (l.tail.isEmpty) None else Some(l.tail),
                                                                       _.head
      )
    val nelUnfoldedHundred: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(List.tabulate(101)(identity))
    assert(cofNelToNel(anaHundred) === nelUnfoldedHundred)
  }

  test("Cofree.tailForced") {
    val spooky = new Spooky
    val incrementor =
      Cofree.unfold[Id, Int](spooky.counter) { _ =>
        spooky.increment(); spooky.counter
      }
    assert(spooky.counter === 0)
    incrementor.tailForced
    assert(spooky.counter === 1)
  }

  test("Cofree.forceTail") {
    val spooky = new Spooky
    val incrementor =
      Cofree.unfold[Id, Int](spooky.counter) { _ =>
        spooky.increment(); spooky.counter
      }
    assert(spooky.counter === 0)
    incrementor.forceTail
    assert(spooky.counter === 1)
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
        }
      )
    assert(spooky.counter === 0)
    incrementor.forceAll
    assert(spooky.counter === 5)
  }

  test("Cofree.mapBranchingRoot") {
    val unfoldedHundred: CofreeNel[Int] = Cofree.unfold[Option, Int](0)(i => if (i == 100) None else Some(i + 1))
    val withNoneRoot = unfoldedHundred.mapBranchingRoot(new (Option ~> Option) {
      def apply[A](a: Option[A]): Option[A] = None
    })
    val nelUnfoldedOne: NonEmptyList[Int] = NonEmptyList.one(0)
    assert(cofNelToNel(withNoneRoot) === nelUnfoldedOne)
  }

  val unfoldedHundred: Cofree[Option, Int] = Cofree.unfold[Option, Int](0)(i => if (i == 100) None else Some(i + 1))
  test("Cofree.mapBranchingS/T") {
    val toList = new (Option ~> List) { def apply[A](a: Option[A]): List[A] = a.toList }
    val toNelS = unfoldedHundred.mapBranchingS(toList)
    val toNelT = unfoldedHundred.mapBranchingT(toList)
    val nelUnfoldedOne: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(List.tabulate(101)(identity))
    assert(cofRoseTreeToNel(toNelS) === nelUnfoldedOne)
    assert(cofRoseTreeToNel(toNelT) === nelUnfoldedOne)
  }

  val nelUnfoldedHundred: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(List.tabulate(101)(identity))

  test("Cofree.cata") {
    val cata =
      Cofree
        .cata[Option, Int, NonEmptyList[Int]](unfoldedHundred)((i, lb) =>
          Eval.now(NonEmptyList(i, lb.fold[List[Int]](Nil)(_.toList)))
        )
        .value
    assert(cata === nelUnfoldedHundred)
  }

  test("Cofree.cata is stack-safe") {
    val unfolded = Cofree.unfold[Option, Int](0)(i => if (i == 50000) None else Some(i + 1))
    val sum = List.tabulate(50000)(identity).sum
    val cata =
      Cofree
        .cata[Option, Int, Int](unfolded)((i, lb) => Eval.now(lb.fold(0)(_ + i)))
        .value

    assert(cata === sum)
  }

  test("Cofree.cataM") {

    type EvalOption[A] = OptionT[Eval, A]

    val folder: (Int, Option[NonEmptyList[Int]]) => EvalOption[NonEmptyList[Int]] =
      (i, lb) => if (i > 100) OptionT.none else OptionT.some(NonEmptyList(i, lb.fold[List[Int]](Nil)(_.toList)))
    val inclusion = OptionT.liftK[Eval]

    val cataHundred =
      Cofree.cataM[Option, EvalOption, Int, NonEmptyList[Int]](unfoldedHundred)(folder)(inclusion).value.value
    val cataHundredOne =
      Cofree
        .cataM[Option, EvalOption, Int, NonEmptyList[Int]](
          Cofree[Option, Int](101, Eval.now(Some(unfoldedHundred)))
        )(folder)(inclusion)
        .value
        .value
    assert(cataHundred === Some(nelUnfoldedHundred))
    assert(cataHundredOne === None)
  }

}

object CofreeSuite extends CofreeSuiteInstances

sealed trait CofreeSuiteInstances {

  type CofreeNel[A] = Cofree[Option, A]
  type CofreeRoseTree[A] = Cofree[List, A]

  implicit def cofNelEq[A](implicit e: Eq[A]): Eq[CofreeNel[A]] = new Eq[CofreeNel[A]] {
    @tailrec def eqv(a: CofreeNel[A], b: CofreeNel[A]): Boolean =
      (a.tailForced, b.tailForced) match {
        case (Some(at), Some(bt)) if e.eqv(a.head, b.head) => eqv(at, bt)
        case (None, None) if e.eqv(a.head, b.head)         => true
        case _                                             => false
      }
  }

  implicit def CofreeOptionCogen[A: Cogen]: Cogen[CofreeNel[A]] =
    implicitly[Cogen[List[A]]].contramap[CofreeNel[A]](cofNelToNel(_).toList)

  implicit def CofreeOptionArb[A: Arbitrary]: Arbitrary[CofreeNel[A]] = {
    val arb = Arbitrary {
      Gen.resize(20, Gen.nonEmptyListOf(implicitly[Arbitrary[A]].arbitrary))
    }
    Arbitrary {
      arb.arbitrary.map(l =>
        (l.head, l.tail) match {
          case (h, Nil) => nelToCofNel(NonEmptyList(h, Nil))
          case (h, t)   => nelToCofNel(NonEmptyList(h, t))
        }
      )
    }
  }

  val nelToCofNel = new (NonEmptyList ~> CofreeNel) {
    def apply[A](fa: NonEmptyList[A]): CofreeNel[A] = Cofree(fa.head, Eval.later(fa.tail.toNel.map(apply)))
  }

  val cofNelToNel =
    new (CofreeNel ~> NonEmptyList) {
      def apply[A](fa: CofreeNel[A]): NonEmptyList[A] =
        NonEmptyList(fa.head, fa.tailForced.map(apply(_).toList).getOrElse(Nil))
    }

  val cofRoseTreeToNel =
    new (CofreeRoseTree ~> NonEmptyList) {
      def apply[A](fa: CofreeRoseTree[A]): NonEmptyList[A] =
        NonEmptyList(fa.head, fa.tailForced.flatMap(apply(_).toList))
    }
}
