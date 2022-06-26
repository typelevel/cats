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

package cats.kernel
package laws

import cats.kernel.instances.int._
import cats.kernel.instances.lazyList._
import cats.kernel.instances.option._
import cats.kernel.laws.discipline.{EqTests, HashTests, MonoidTests, OrderTests, PartialOrderTests, SerializableTests}

trait ScalaVersionSpecificTests { this: Tests =>
  checkAll("Eq[LazyList[HasEq[Int]]]", EqTests[LazyList[HasEq[Int]]].eqv)
  checkAll("PartialOrder[LazyList[HasPartialOrder[Int]]]",
           PartialOrderTests[LazyList[HasPartialOrder[Int]]].partialOrder
  )
  checkAll("Order[LazyList[Int]]", OrderTests[LazyList[Int]].order)
  checkAll("Monoid[LazyList[Int]]", MonoidTests[LazyList[Int]].monoid)
  checkAll("Monoid[LazyList[Int]]", SerializableTests.serializable(Monoid[LazyList[Int]]))
  checkAll("Hash[LazyList[Int]]", HashTests[LazyList[Int]].hash)
}
