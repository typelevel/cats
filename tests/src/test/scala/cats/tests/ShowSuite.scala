package cats
package tests

import java.util.concurrent.TimeUnit

import cats.Show.ContravariantShow
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{ContravariantTests, SerializableTests}
import cats.laws.discipline.eq._
import org.scalatest.FunSuite

import scala.concurrent.duration.{Duration, FiniteDuration}

class ShowSuite extends CatsSuite {
  checkAll("Contravariant[Show]", ContravariantTests[Show].contravariant[Int, Int, Int])
  checkAll("Contravariant[Show]", SerializableTests.serializable(Contravariant[Show]))

  sealed trait TimeOfDay
  case object Morning extends TimeOfDay
  object TimeOfDay {
    implicit val showTimeOfDay: Show[TimeOfDay] = Show.show { case Morning => "morning" }
  }

  test("show string interpolator") {
    case class Cat(name: String)
    object Cat {
      implicit val showCat: Show[Cat] = Show.show(_.name)
    }
    val tod: TimeOfDay = Morning
    val cat = Cat("Whiskers")

    assertResult("Good morning, Whiskers!")(show"Good $tod, $cat!")

    assertResult("Good morning, Whiskers!")(show"Good $tod, ${List(cat).head}!")
  }

  test("show string interpolator and contravariance") {
    val tod: Morning.type = Morning

    assertResult("Good morning")(show"Good $tod")
  }

  test("contravariant show is not ambiguous when both FiniteDuration's and Duration's instances are in scope") {
    implicitly[ContravariantShow[FiniteDuration]]
    implicitly[ContravariantShow[Duration]]
  }

  test("show interpolation works when both FiniteDuration's and Duration's instances are in scope") {
    show"instance resolution is not ambiguous for ${FiniteDuration(3L, TimeUnit.SECONDS)}"
    show"instance resolution is not ambiguous for ${Duration(3L, TimeUnit.SECONDS)}"
  }
}

final class ShowSuite2 extends FunSuite {

  test(
    "contravariant show for FiniteDuration can be inferred when importing both duration's and finiteDuration's instances"
  ) {

    import cats.instances.duration._
    import cats.instances.finiteDuration._

    implicitly[Order[Duration]]
    implicitly[Order[FiniteDuration]]

    implicitly[ContravariantShow[FiniteDuration]]
  }

  test("all the Duration's and FiniteDuration's instances can be correctly inferred when importing implicits") {

    import cats.implicits._

    implicitly[Order[Duration]]
    implicitly[Order[FiniteDuration]]

    implicitly[ContravariantShow[Duration]]
    implicitly[ContravariantShow[FiniteDuration]]
  }
}
