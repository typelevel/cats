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

import cats.kernel.Eq
import cats.laws.discipline.{CategoryTests, SerializableTests}
import org.scalacheck.{Arbitrary, Gen}
import cats.arrow.Category

class AsSuite extends CatsSuite {
  import cats.evidence.*

  def toMap[A, B, X](fa: List[X])(implicit ev: X <~< (A, B)): Map[A, B] = {
    type RequiredFunc = (Map[A, B], X) => Map[A, B]
    type GivenFunc = (Map[A, B], (A, B)) => Map[A, B]
    val subst: GivenFunc <~< RequiredFunc = As.contra2_3(ev) // because inference failed on Scala.js on 2.10.6
    fa.foldLeft(Map.empty[A, B])(subst(_ + _))
  }

  implicit def arbAs[A, B](implicit ev: A <~< B): Arbitrary[A <~< B] = Arbitrary(Gen.const(ev))
  implicit def eq[A, B]: Eq[As[A, B]] = Eq.fromUniversalEquals

  test("narrow an input of a function2") {
    // scala's GenTraversableOnce#toMap has a similar <:< constraint

    toMap(List("String" -> 1))
  }

  test("lift <:") {
    // scala's GenTraversableOnce#toMap has a similar <:< constraint
    trait Bar
    case class Foo(x: Int) extends Bar

    val lifted: Foo <~< Bar = As.reify[Foo, Bar]
    toMap(List("String" -> Foo(1)))(As.co2_2(lifted))
  }

  test("check expected relationships") {
    // scala's GenTraversableOnce#toMap has a similar <:< constraint
    implicitly[Int <~< Any]
    implicitly[String <~< Any]
    implicitly[String <~< AnyRef]
    implicitly[String <~< AnyRef]
    implicitly[(String, Int) <~< (AnyRef, Any)]
    implicitly[scala.collection.immutable.List[String] <~< scala.collection.Seq[Any]]

    {
      trait Foo
      trait Bar
      implicit def subFooBar: Foo <:< Bar = implicitly[Foo <:< Foo].asInstanceOf[Foo <:< Bar]
      // make sure the above is found
      implicitly[As[Foo, Bar]]
      val res: Foo <:< Bar = implicitly[As[Foo, Bar]].toPredef
    }
  }

  trait Top {
    def foo: String = this.getClass.getName
  }
  trait Middle extends Top
  case class Bottom() extends Middle

  checkAll("As[Bottom, Middle]", CategoryTests[As].category[Bottom, Middle, Top, Any])
  checkAll("Category[As]", SerializableTests.serializable(Category[As]))

  test("subtyping relationships compose") {

    val cAsB: Bottom As Middle = As.reify[Bottom, Middle]
    val bAsA: Middle As Top = As.fromPredef(implicitly)

    val one: Bottom As Top = cAsB.andThen(bAsA)
    val two: Bottom As Top = bAsA.compose(cAsB)
  }

  test("we can use As to coerce a value") {
    val cAsA: Bottom As Top = implicitly

    val c: Bottom = Bottom()

    val a: Top = cAsA.coerce(c)
    a.foo
  }

  test("we can lift subtyping to covariant type constructors") {
    val cAsA: Bottom As Top = implicitly
    val co: List[Bottom] As List[Top] = As.co(cAsA)
    val co2: (Bottom, String) As (Top, String) = As.co2(cAsA)
    val co2_2: (String, Bottom) As (String, Top) = As.co2_2(cAsA)
    val co3: (Bottom, Unit, Unit) As (Top, Unit, Unit) = As.co3(cAsA)
    val co3_2: (Unit, Bottom, Unit) As (Unit, Top, Unit) = As.co3_2(cAsA)
    val co3_3: (Unit, Unit, Bottom) As (Unit, Unit, Top) = As.co3_3(cAsA)
    val lift2: (Bottom, String) As (Top, Any) = As.lift2(cAsA, implicitly)
  }

  test("we can lift subtyping to contravariant type constructors") {
    type Eat[-A] = A => Unit
    type EatF[-A, B] = A => B
    type Eatꟻ[B, -A] = A => B
    type EatF13[-A, B, C] = A => (B, C)
    type EatF23[B, -A, C] = A => (B, C)
    type EatF33[B, C, -A] = A => (B, C)

    val cAsA: Bottom As Top = implicitly
    val contra: Eat[Top] As Eat[Bottom] = As.contra(cAsA)
    val contra1_2: EatF[Top, Unit] As EatF[Bottom, Unit] = As.contra1_2(cAsA)
    val contra2_2: Eatꟻ[Unit, Top] As Eatꟻ[Unit, Bottom] = As.contra2_2(cAsA)
    val contra1_3: EatF13[Top, Unit, Unit] As EatF13[Bottom, Unit, Unit] = As.contra1_3(cAsA)
    val contra2_3: EatF23[Unit, Top, Unit] As EatF23[Unit, Bottom, Unit] = As.contra2_3(cAsA)
    val contra3_3: EatF33[Unit, Unit, Top] As EatF33[Unit, Unit, Bottom] = As.contra3_3(cAsA)
  }

  test("we can widen the output of a function1") {
    val f: Any => Bottom = _ => Bottom()
    val cAsA: Bottom As Top = implicitly
    val f2: Any => Top = As.onF(cAsA)(f)
  }

  test("we can narrow the input of a function1") {
    val f: Top => Any = (t: Top) => t
    val cAsA: Bottom As Top = implicitly
    val f2: Bottom => Any = As.conF(cAsA)(f)
  }

  test("we can simultaneously narrow the input and widen the output of a Function1") {
    val f: Top => Bottom = _ => Bottom()
    val cAsA: Bottom As Top = implicitly
    val f2: Bottom => Top = As.invF(cAsA, cAsA)(f)
  }
}
