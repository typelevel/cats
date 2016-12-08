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
    import Show._

    case class Cat(name: String)
    object Cat {
      implicit val showCat: Show[Cat] = new Show[Cat] {
        def show(cat: Cat): String = cat.name
      }
    }

    sealed trait TimeOfDay
    case object Morning extends TimeOfDay
    object TimeOfDay {
      implicit val showTimeOfDay: Show[TimeOfDay] = new Show[TimeOfDay] {
        def show(tod: TimeOfDay): String = tod match {
          case Morning => "morning"
        }
      }
    }

    val tod: TimeOfDay = Morning
    val cat = Cat("Whiskers")

    val greeting = show"Good $tod, $cat!"

    assertResult("Good morning, Whiskers!")(greeting)
  }
}
