package cats
package tests

import cats.data._
import cats.laws.discipline.SerializableTests

// the things we assert here are not so much important, what is
// important is that this stuff compiles at all.
class UnapplyTests extends CatsSuite {

  test("Unapply works for stuff already the right kind") {
    val x = Traverse[List].traverseU(List(1,2,3))(Option(_))
    x should === (Some(List(1,2,3)))
  }

  test("Unapply works for F[_,_] with the left fixed") {
    val x = Traverse[List].traverseU(List(1,2,3))(Either.right(_))
    (x: Either[String, List[Int]]) should === (Either.right(List(1,2,3)))
  }

  test("Unapply works for F[_[_],_] with the left fixed") {
    val x: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
    val y: OptionT[List, Int] = OptionT(List(Option(3), Option(4)))

    val z: List[Option[(Int,Int)]] = (x |@| y).tupled.value

    z should be (List(Option((1,3)), Option((1,4)),
                      Option((2,3)), Option((2,4))))
  }

  checkAll("Unapply[Functor, Option[String]]", SerializableTests.serializable(Unapply[Functor, Option[String]]))

  test("Unapply works for F[_[_], _[_], _]") {
    val x: List[Option[Int]] = List(Option(1), Option(2))
    val y: Nested[List, Option, Int] = Nested(x)

    y.map(_ + 1).value should === (x.map(_.map(_ + 1)))
  }
}
