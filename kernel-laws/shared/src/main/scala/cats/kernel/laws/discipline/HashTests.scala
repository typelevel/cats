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

package cats
package kernel
package laws
package discipline

import cats.kernel.instances.boolean._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

import scala.util.hashing.Hashing

trait HashTests[A] extends EqTests[A] {

  def laws: HashLaws[A]

  def hash(implicit arbA: Arbitrary[A], arbF: Arbitrary[A => A]): RuleSet =
    new DefaultRuleSet(
      "hash",
      Some(eqv),
      "hash compatibility" -> forAll(laws.hashCompatibility _)
    )

  @deprecated("use `hash` without `Hashing` parameter", "2.12.1")
  def hash(arbA: Arbitrary[A], arbF: Arbitrary[A => A], eqA: Eq[A], hashA: Hashing[A]): RuleSet =
    hash(arbA, arbF)
}

object HashTests {
  def apply[A: Hash]: HashTests[A] =
    new HashTests[A] { def laws: HashLaws[A] = HashLaws[A] }
}
