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

package cats.tests

import cats.{Contravariant, Show}
import cats.Show.ContravariantShow
import cats.kernel.Order
import cats.syntax.show._
import cats.laws.discipline.{ContravariantTests, DeferTests, MiniInt, SerializableTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

import java.util.concurrent.TimeUnit
import scala.collection.immutable.Seq
import scala.concurrent.duration.{Duration, FiniteDuration}

class ShowSuite extends CatsSuite {
  checkAll("Contravariant[Show]", ContravariantTests[Show].contravariant[MiniInt, Int, Boolean])
  checkAll("Contravariant[Show]", SerializableTests.serializable(Contravariant[Show]))
  checkAll("Defer[Show]", DeferTests[Show].defer[MiniInt])

  sealed trait TimeOfDay
  case object Morning extends TimeOfDay
  object TimeOfDay {
    implicit val showTimeOfDay: Show[TimeOfDay] = { case Morning => "morning" }
  }

  test("show string interpolator") {
    case class Cat(name: String)
    object Cat {
      implicit val showCat: Show[Cat] = _.name
    }
    val tod: TimeOfDay = Morning
    val cat = Cat("Whiskers")

    assertEquals("Good morning, Whiskers!", show"Good $tod, $cat!")

    assertEquals("Good morning, Whiskers!", show"Good $tod, ${List(cat).head}!")
  }

  test("show string interpolator and contravariance") {
    val tod: Morning.type = Morning

    assertEquals("Good morning", show"Good $tod")
  }

  test("contravariant show is not ambiguous when both FiniteDuration's and Duration's instances are in scope") {
    implicitly[ContravariantShow[FiniteDuration]]
    implicitly[ContravariantShow[Duration]]
  }

  test("show interpolation works when both FiniteDuration's and Duration's instances are in scope") {
    show"instance resolution is not ambiguous for ${FiniteDuration(3L, TimeUnit.SECONDS)}"
    show"instance resolution is not ambiguous for ${Duration(3L, TimeUnit.SECONDS)}"
  }

  test("show interpolation with Seq subtypes isn't ambiguous") {
    implicitly[ContravariantShow[Seq[Int]]]
    implicitly[ContravariantShow[List[Int]]]
    implicitly[ContravariantShow[Vector[Int]]]

    val goodmornings = Seq("guten Tag", "good morning", "bonjour")
    assertEquals(show"$goodmornings", "List(guten Tag, good morning, bonjour)")
    assertEquals(show"${goodmornings.toList}", "List(guten Tag, good morning, bonjour)")
    assertEquals(show"${goodmornings.toVector}", "Vector(guten Tag, good morning, bonjour)")
  }
}

final class ShowSuite2 extends munit.FunSuite {

  test(
    "contravariant show for FiniteDuration can be inferred when importing both duration's and finiteDuration's instances"
  ) {

    implicitly[Order[Duration]]
    implicitly[Order[FiniteDuration]]

    implicitly[ContravariantShow[FiniteDuration]]
  }

  test("all the Duration's and FiniteDuration's instances can be correctly inferred when importing implicits") {

    implicitly[Order[Duration]]
    implicitly[Order[FiniteDuration]]

    implicitly[ContravariantShow[Duration]]
    implicitly[ContravariantShow[FiniteDuration]]
  }
}
