package cats.tests

import cats.Fold.Continue
import cats.Lazy
import cats.data.IList.{ICons, INil}
import cats.data.{IList, OneAnd}
import cats.laws.discipline.FunctorTests
import cats.laws.discipline.arbitrary._
import cats.std.int._
import cats.std.option._
import cats.std.list._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.typelevel.discipline.scalatest.Discipline

class IListTests extends FunSuite with Checkers with Discipline {

  test("apply") {
    assert(IList(1,2,3) === ICons(1, ICons(2, ICons(3, INil()))))
  }

  test("fromFoldable") {
    assert(IList.fromFoldable(List(1,2,3)) === IList(1,2,3))
  }

  test("toList") {
    check((as: IList[Int]) =>
      IList.fromFoldable(as.toList) === as
    )
  }

  test("foldLeft with cons reverse the list") {
    check((as: IList[Int]) =>
      as.foldLeft(IList.empty[Int])(_.cons(_)).toList === as.toList.reverse
    )
  }

  test("foldMap") {
    check((as: IList[Int]) =>
      as.foldMap(IList.empty[Int])(IList.singleton) === as
    )
  }

  test("flatMap") {
    check((as: IList[Int]) =>
      as.flatMap(IList.singleton) === as
    )
  }

  test("foldLazy") {
    assert(IList(1,2,3).foldLazy(Lazy.eager(0))(i => Continue[Int](_ + i)).force == 6)
  }

  test("show") {
    assert(IList(1,2,3).show === "[1,2,3]")
  }

  test("cons - uncons") {
    check((h: Int, t: IList[Int]) =>
      (h :: t).toNel === Some(OneAnd(h, t))
    )
  }

  test("append") {
    check((xs: IList[Int], ys: IList[Int]) =>
      xs ::: ys === IList.fromFoldable(xs.toList ::: ys.toList)
    )
  }

  test("filter"){
    assert(IList(1,2,3,4,5).filter(_ <= 2) === IList(1,2))
  }

  test("lookup"){
    assert(IList(-1,0,1,2,3).map(i => IList(0,1,2).lookup(i)) === IList(None, Some(0), Some(1), Some(2), None))
  }

  test("take"){
    assert(IList(1,2,3,4,5).take(2) === IList(1,2))
  }

  test("takeWhile"){
    assert(IList(1,2,3,4,5).takeWhile(_ <= 3) === IList(1,2,3))
  }

  test("drop"){
    assert(IList(1,2,3,4,5).drop(2) === IList(3,4,5))
  }

  test("dropWhile"){
    assert(IList(1,2,3,4,5).dropWhile(_ <= 3) === IList(4,5))
  }

  test("widen"){
    sealed trait Fruit
    case object Apple  extends Fruit
    case object Orange extends Fruit

    val apples: IList[Apple.type] = IList(Apple, Apple)

    // Orange :: apples doesn't compile
    assert((Orange :: apples.widen[Fruit]) == IList[Fruit](Orange, Apple, Apple))
  }

  checkAll("IList[Int]", FunctorTests[IList, Int].applicative[Int, Int])

}
