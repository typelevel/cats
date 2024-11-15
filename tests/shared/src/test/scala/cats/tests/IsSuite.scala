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

import cats.arrow.*
import cats.evidence.{Is, Leibniz}
import cats.kernel.Eq
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.CategoryTests
import org.scalacheck.{Arbitrary, Gen}

class IsSuite extends CatsSuite {
  implicit def arbIs[A, B](implicit ev: A Is B): Arbitrary[A Is B] = Arbitrary(Gen.const(ev))
  implicit def eqIs[A, B]: Eq[A Is B] = Eq.fromUniversalEquals

  trait Top {
    def foo: String = this.getClass.getName
  }
  case class Bottom() extends Top

  checkAll("Is[Bottom, Bottom]", CategoryTests[Is].category[Bottom, Bottom, Bottom, Bottom])
  checkAll("Category[Is]", SerializableTests.serializable(Category[Is]))

  test("syntax") {
    trait Bar

    val lifted: Bar Is Bar = Is.refl[Bar]
    val andThen: Leibniz[Bar, Bar] = lifted.andThen(lifted)
    val compose: Leibniz[Bar, Bar] = lifted.compose(lifted)
    val flip: Leibniz[Bar, Bar] = lifted.flip
    val lift: Leibniz[List[Bar], List[Bar]] = lifted.lift[List]
    val coerce: Bar = lifted.coerce(new Bar {})
    val predefEq: =:=[Bar, Bar] = lifted.toPredef

    {
      trait Foo
      implicit def eqFooBar: Foo =:= Bar = implicitly[Foo =:= Foo].asInstanceOf[Foo =:= Bar]
      // make sure the above is found
      implicitly[Is[Foo, Bar]]

      val res: Foo =:= Bar = implicitly[Is[Foo, Bar]].toPredef
    }
  }

}
