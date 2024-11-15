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

import cats.Traverse
import cats.TraverseFilter
import cats.data.Chain
import cats.laws.discipline.arbitrary.catsLawsArbitraryForChain
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.collection.immutable.Queue

abstract class TraverseFilterSuite[F[_]: TraverseFilter](name: String)(implicit
  ArbFInt: Arbitrary[F[Int]],
  ArbFString: Arbitrary[F[String]]
) extends CatsSuite {

  implicit def T: Traverse[F] = implicitly[TraverseFilter[F]].traverse

  test(s"TraverseFilter[$name].ordDistinct") {
    forAll { (fa: F[Int]) =>
      fa.ordDistinct.toList === fa.toList.distinct
    }
  }

  test(s"TraverseFilter[$name].hashDistinct") {
    forAll { (fa: F[String]) =>
      fa.hashDistinct.toList === fa.toList.distinct
    }
  }
}

class TraverseFilterListSuite extends TraverseFilterSuite[List]("list")

class TraverseFilterVectorSuite extends TraverseFilterSuite[Vector]("vector")

class TraverseFilterChainSuite extends TraverseFilterSuite[Chain]("chain")

class TraverseFilterQueueSuite extends TraverseFilterSuite[Queue]("queue")

@annotation.nowarn("cat=deprecation")
class TraverseFilterStreamSuite extends TraverseFilterSuite[Stream]("stream")
