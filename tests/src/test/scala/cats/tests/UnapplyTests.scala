package cats
package tests

import cats.data._
import cats.laws.discipline.SerializableTests

// the things we assert here are not so much important, what is
// important is that this stuff compiles at all.
class UnapplyTests extends CatsSuite {


  test("Unapply works for F[_] ") {

    val u = implicitly[Unapply.Aux1[Functor, Option[Int], Option, Int]]

    u.TC.map(u.subst(Option(1)))(_ + 1) should ===(Option(2))
  }

  test("Unapply works for F[_, _] with left fixed ") {

    val u = implicitly[Unapply.Aux1[Functor, Either[String, Int], Either[String, ?], Int]]

    u.TC.map(u.subst(1.asRight[String]))(_ + 1) should ===(2.asRight[String])
  }

  test("Unapply works for F[_[_],_] with the left fixed") {

    val u = implicitly[Unapply.Aux1[Functor, OptionT[List, Int], OptionT[List, ?], Int]]

    u.TC.map(u.subst(OptionT(List(Option(1)))))(_ + 1) should ===(OptionT(List(Option(2))))
  }

  checkAll("Unapply[Functor, Option[String]]", SerializableTests.serializable(Unapply[Functor, Option[String]]))

  test("Unapply works for F[_[_], _[_], _]") {
    val x: List[Option[Int]] = List(Option(1), Option(2))
    val y: Nested[List, Option, Int] = Nested(x)

    val u = implicitly[Unapply.Aux1[Functor, Nested[List, Option, Int], Nested[List, Option, ?], Int]]

    u.TC.map(u.subst(y))(_ + 1).value should ===(x.map(_.map(_ + 1)))
  }
}
