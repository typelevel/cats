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

import cats.data.NonEmptySeq
import cats.laws.discipline.arbitrary._
import cats.syntax.seq._
import org.scalacheck.Prop._

import scala.collection.immutable.Seq

class NonEmptySeqSuite extends NonEmptyCollectionSuite[Seq, NonEmptySeq, NonEmptySeq] {
  protected def toList[A](value: NonEmptySeq[A]): List[A] = value.toSeq.toList
  protected def underlyingToList[A](underlying: Seq[A]): List[A] = underlying.toList
  protected def toNonEmptyCollection[A](value: NonEmptySeq[A]): NonEmptySeq[A] = value

  test("neSeq => Seq => neSeq returns original neSeq")(
    forAll { (fa: NonEmptySeq[Int]) =>
      assert(fa.toSeq.toNeSeq == Some(fa))
    }
  )

  test("NonEmptySeq#concat/appendSeq is consistent with Seq#++")(
    forAll { (fa: NonEmptySeq[Int], fb: Seq[Int]) =>
      assert((fa ++ fb).toSeq == fa.toSeq ++ fb)
      assert(fa.concat(fb).toSeq == fa.toSeq ++ fb)
      assert(fa.appendSeq(fb).toSeq == fa.toSeq ++ fb)
    }
  )

  test("NonEmptySeq#concatNeSeq is consistent with concat")(
    forAll { (fa: NonEmptySeq[Int], fb: NonEmptySeq[Int]) =>
      assert(fa.concatNeSeq(fb).toSeq == fa.concat(fb.toSeq).toSeq)
    }
  )

  test("toNeSeq on empty Seq returns None") {
    assert(Seq.empty[Int].toNeSeq == None)
  }
}
