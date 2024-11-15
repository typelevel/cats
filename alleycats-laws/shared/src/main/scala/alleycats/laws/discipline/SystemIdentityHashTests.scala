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

package alleycats.laws.discipline

import cats.kernel.{Eq, Hash}
import cats.kernel.laws.HashLaws
import cats.kernel.laws.discipline.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.util.hashing.Hashing

trait SystemIdentityHashTests[A] extends ReferentialEqTests[A] {
  def laws: HashLaws[A]

  def hash(implicit arbA: Arbitrary[A], eqA: Eq[A], hashA: Hashing[A]): RuleSet =
    new DefaultRuleSet(
      "systemIdentityHash",
      Some(eqv),
      "hash compatibility" -> forAll(laws.hashCompatibility _),
      "same as universal hash" -> forAll(laws.sameAsUniversalHash _),
      "same as scala hashing" -> forAll((x: A, y: A) => laws.sameAsScalaHashing(x, y, hashA))
    )
}

object SystemIdentityHashTests {
  def apply[A: Hash]: SystemIdentityHashTests[A] = new SystemIdentityHashTests[A] {
    override def laws: HashLaws[A] = HashLaws[A]
  }
}
