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

import cats._
import cats.data.NonEmptySeq
import cats.kernel.laws.discipline.SemigroupTests
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.all._
import org.scalacheck.Prop._

import scala.collection.immutable.Seq

class NonEmptySeqSuite extends NonEmptyCollectionSuite[Seq, NonEmptySeq, NonEmptySeq] {
  override protected def toList[A](value: NonEmptySeq[A]): List[A] = value.toList
  override protected def underlyingToList[A](underlying: Seq[A]): List[A] = underlying.toList
  override protected def toNonEmptyCollection[A](value: NonEmptySeq[A]): NonEmptySeq[A] = value

  checkAll(
    "NonEmptySeq[Int] with Option",
    NonEmptyTraverseTests[NonEmptySeq].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll("NonEmptyTraverse[NonEmptySeq[A]]", SerializableTests.serializable(NonEmptyTraverse[NonEmptySeq]))

  checkAll("NonEmptySeq[Int]", ReducibleTests[NonEmptySeq].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptySeq]", SerializableTests.serializable(Reducible[NonEmptySeq]))

  checkAll("NonEmptySeq[Int]", NonEmptyAlternativeTests[NonEmptySeq].nonEmptyAlternative[Int, Int, Int])
  checkAll("NonEmptyAlternative[NonEmptySeq[A]]", SerializableTests.serializable(NonEmptyAlternative[NonEmptySeq]))

  checkAll("NonEmptySeq[Int]", SemigroupTests[NonEmptySeq[Int]].semigroup)
  checkAll("Semigroup[NonEmptySeq[Int]]", SerializableTests.serializable(Semigroup[NonEmptySeq[Int]]))

  checkAll("NonEmptySeq[Int]", BimonadTests[NonEmptySeq].bimonad[Int, Int, Int])
  checkAll("Bimonad[NonEmptySeq]", SerializableTests.serializable(Bimonad[NonEmptySeq]))

  checkAll("NonEmptySeq[Int]", AlignTests[NonEmptySeq].align[Int, Int, Int, Int])
  checkAll("Align[NonEmptySeq]", SerializableTests.serializable(Align[NonEmptySeq]))

  checkAll("NonEmptySeq[Int]", ShortCircuitingTests[NonEmptySeq].foldable[Int])
  checkAll("NonEmptySeq[Int]", ShortCircuitingTests[NonEmptySeq].traverse[Int])
  checkAll("NonEmptySeq[Int]", ShortCircuitingTests[NonEmptySeq].nonEmptyTraverse[Int])

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
