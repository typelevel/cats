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

import cats.{Align, Alternative, CoflatMap, Eval, Monad, Semigroupal, Traverse, TraverseFilter}
import cats.data.ZipSeq
import cats.laws.discipline.{
  AlignTests,
  AlternativeTests,
  CoflatMapTests,
  CommutativeApplyTests,
  MonadTests,
  SemigroupalTests,
  SerializableTests,
  ShortCircuitingTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.arbitrary._
import cats.syntax.show._
import cats.syntax.eq._
import org.scalacheck.Prop._
import scala.collection.immutable.Seq

class SeqSuite extends CatsSuite {
  checkAll("Seq[Int]", SemigroupalTests[Seq].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Seq]", SerializableTests.serializable(Semigroupal[Seq]))

  checkAll("Seq[Int]", CoflatMapTests[Seq].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Seq]", SerializableTests.serializable(CoflatMap[Seq]))

  checkAll("Seq[Int]", AlternativeTests[Seq].alternative[Int, Int, Int])
  checkAll("Alternative[Seq]", SerializableTests.serializable(Alternative[Seq]))

  // Traverse behaviour discriminates on the Runtime type of the Applicative
  checkAll("Seq[Int] with Option", TraverseTests[Seq].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Seq[Int] with Eval", TraverseTests[Seq].traverse[Int, Int, Int, Set[Int], Eval, Eval])
  checkAll("Traverse[Seq]", SerializableTests.serializable(Traverse[Seq]))

  checkAll("Seq[Int]", MonadTests[Seq].monad[Int, Int, Int])
  checkAll("Monad[Seq]", SerializableTests.serializable(Monad[Seq]))

  checkAll("Seq[Int]", TraverseFilterTests[Seq].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Seq]", SerializableTests.serializable(TraverseFilter[Seq]))

  checkAll("Seq[Int]", AlignTests[Seq].align[Int, Int, Int, Int])
  checkAll("Align[Seq]", SerializableTests.serializable(Align[Seq]))

  checkAll("Seq[Int]", ShortCircuitingTests[Seq].traverseFilter[Int])
  checkAll("Seq[Int]", ShortCircuitingTests[Seq].foldable[Int])

  checkAll("ZipSeq[Int]", CommutativeApplyTests[ZipSeq].commutativeApply[Int, Int, Int])

  test("show") {
    forAll { (seq: Seq[String]) =>
      assert(seq.show === (seq.toString))
    }
  }

  test("traverse is stack-safe") {
    val seq = (0 until 100000).toSeq
    val sumAll = Traverse[Seq]
      .traverse(seq) { i => () => i }
      .apply()
      .sum

    assert(sumAll == seq.sum)
  }
}

final class SeqInstancesSuite extends munit.FunSuite {

  test("NonEmptyParallel instance in cats.instances.seq") {
    import cats.instances.seq._
    import cats.syntax.parallel._

    (Seq(1, 2, 3), Seq("A", "B", "C")).parTupled
  }
}
