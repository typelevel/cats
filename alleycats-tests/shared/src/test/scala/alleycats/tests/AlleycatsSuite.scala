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

package alleycats.tests

import alleycats.std.MapInstances
import cats._
import cats.instances.all._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Test.Parameters

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Alleycats tests. Derived from Cats.
 */
trait AlleycatsSuite extends munit.DisciplineSuite with TestSettings with TestInstances with MapInstances {
  implicit override def scalaCheckTestParameters: Parameters =
    checkConfiguration

  implicit def EqIterable[A: Eq]: Eq[Iterable[A]] = Eq.by(_.toList)
}

sealed trait TestInstances {
  implicit val arbObject: Arbitrary[Object] =
    // with some probability we select from a small set of objects
    // otherwise make a totally new one
    // courtesy of @johnynek
    Arbitrary(
      Gen.oneOf(
        Gen.oneOf(List.fill(5)(new Object)),
        Arbitrary.arbUnit.arbitrary.map(_ => new Object)
      )
    )
}
