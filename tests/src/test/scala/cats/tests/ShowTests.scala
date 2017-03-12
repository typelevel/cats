package cats
package tests

import cats.functor.Contravariant
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{ContravariantTests, SerializableTests}
import cats.laws.discipline.eq._

class ShowTests extends CatsSuite {
  checkAll("Contravariant[Show]", ContravariantTests[Show].contravariant[Int, Int, Int])
  checkAll("Contravariant[Show]", SerializableTests.serializable(Contravariant[Show]))

  test("show string interpolator") {
    import cats.syntax.show._

    case class Cat(name: String)
    object Cat {
      implicit val showCat: Show[Cat] = Show.show(_.name)
    }

    sealed trait TimeOfDay
    case object Morning extends TimeOfDay
    object TimeOfDay {
      implicit val showTimeOfDay: Show[TimeOfDay] = Show.show { case Morning => "morning" }
    }

    val tod: TimeOfDay = Morning
    val cat = Cat("Whiskers")

    assertResult("Good morning, Whiskers!")(show"Good $tod, $cat!")

    assertResult("Good morning, Whiskers!")(show"Good $tod, ${List(cat).head}!")
  }
}
