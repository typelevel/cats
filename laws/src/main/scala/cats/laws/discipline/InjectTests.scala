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
package laws
package discipline

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop.*
import org.typelevel.discipline.Laws

trait InjectTests[A, B] extends Laws {
  def laws: InjectLaws[A, B]

  def inject(implicit
    ArbA: Arbitrary[A],
    EqOptionA: Eq[Option[A]],
    ArbB: Arbitrary[B],
    EqOptionB: Eq[Option[B]]
  ): RuleSet =
    new DefaultRuleSet(
      "inject",
      None,
      "inject round trip inj" -> forAll((a: A) => laws.injectRoundTripInj(a)),
      "inject round trip prj" -> forAll((b: B) => laws.injectRoundTripPrj(b))
    )

}

object InjectTests {
  def apply[A, B](implicit ev: Inject[A, B]): InjectTests[A, B] =
    new InjectTests[A, B] { val laws: InjectLaws[A, B] = InjectLaws[A, B] }
}
