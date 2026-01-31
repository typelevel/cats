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

package cats.laws.discipline

import cats.data.{NonEmptyLazyList, ZipLazyList, ZipStream}
import org.scalacheck.{Arbitrary, Cogen}

private[discipline] object ScalaVersionSpecific {

  trait ArbitraryInstances {
    @deprecated("Use catsLawsArbitraryForZipLazyList", "2.0.0-RC2")
    implicit def catsLawsArbitraryForZipStream[A](implicit A: Arbitrary[A]): Arbitrary[ZipStream[A]] =
      Arbitrary(implicitly[Arbitrary[Stream[A]]].arbitrary.map(v => new ZipStream(v)))

    implicit def catsLawsArbitraryForZipLazyList[A](implicit A: Arbitrary[A]): Arbitrary[ZipLazyList[A]] =
      Arbitrary(implicitly[Arbitrary[LazyList[A]]].arbitrary.map(v => new ZipLazyList(v)))

    implicit def catsLawsArbitraryForNonEmptyLazyList[A](implicit A: Arbitrary[A]): Arbitrary[NonEmptyLazyList[A]] =
      Arbitrary(
        implicitly[Arbitrary[LazyList[A]]].arbitrary
          .flatMap(fa => A.arbitrary.map(a => NonEmptyLazyList.fromLazyListPrepend(a, fa)))
      )

    implicit def catsLawsCogenForNonEmptyLazyList[A](implicit A: Cogen[A]): Cogen[NonEmptyLazyList[A]] =
      Cogen[LazyList[A]].contramap(_.toLazyList)
  }
}
