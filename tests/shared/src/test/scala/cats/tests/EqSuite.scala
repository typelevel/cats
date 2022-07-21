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

import cats.{Contravariant, ContravariantMonoidal, ContravariantSemigroupal, Decidable, Invariant, Semigroupal}
import cats.kernel.Eq
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.{DecidableTests, MiniInt}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class EqSuite extends CatsSuite {
  Invariant[Eq]
  Contravariant[Eq]
  Semigroupal[Eq]
  ContravariantSemigroupal[Eq]
  ContravariantMonoidal[Eq]
  Decidable[Eq]

  // Since equivalence on functions (Nothing, Nothing) => Boolean is
  // equivalence on Nothing => Boolean it should be vacuously true
  implicit val eqForEqNothing: Eq[Eq[Nothing]] = Eq.allEqual[Eq[Nothing]]
  checkAll("Eq", DecidableTests[Eq].decidable[MiniInt, Boolean, Boolean])
  checkAll("Decidable[Eq]", SerializableTests.serializable(Decidable[Eq]))

}
