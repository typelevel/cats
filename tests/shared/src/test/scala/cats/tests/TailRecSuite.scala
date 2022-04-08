/*
 * Copyright (c) 2022 Typelevel
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

import cats.{Defer, Monad}
import cats.kernel.Eq
import cats.laws.discipline.{DeferTests, MonadTests, SerializableTests}
import scala.util.control.TailCalls.{done, tailcall, TailRec}
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.arbitrary

class TailRecSuite extends CatsSuite {

  implicit def tailRecArb[A: Arbitrary: Cogen]: Arbitrary[TailRec[A]] =
    Arbitrary(
      Gen.frequency(
        (3, arbitrary[A].map(done(_))),
        (1, Gen.lzy(arbitrary[(A, A => TailRec[A])].map { case (a, fn) => tailcall(fn(a)) })),
        (1, Gen.lzy(arbitrary[(TailRec[A], A => TailRec[A])].map { case (a, fn) => a.flatMap(fn) }))
      )
    )

  implicit def eqTailRec[A: Eq]: Eq[TailRec[A]] =
    Eq.by[TailRec[A], A](_.result)

  checkAll("TailRec[Int]", MonadTests[TailRec].monad[Int, Int, Int])
  checkAll("Monad[TailRec]", SerializableTests.serializable(Monad[TailRec]))

  checkAll("TailRec[Int]", DeferTests[TailRec].defer[Int])
  checkAll("Defer[TailRec]", SerializableTests.serializable(Defer[TailRec]))
}
