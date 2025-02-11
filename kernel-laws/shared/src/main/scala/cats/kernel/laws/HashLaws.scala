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

import scala.util.hashing.*

trait HashLaws[A] extends EqLaws[A] {
  implicit override def E: Hash[A]

  def hashCompatibility(x: A, y: A): IsEq[Boolean] =
    (!E.eqv(x, y) || (Hash.hash(x) == Hash.hash(y))) <-> true

  @deprecated("This law is no longer enforced", "2.9.0")
  def sameAsUniversalHash(x: A, y: A): IsEq[Boolean] =
    ((E.hash(x) == x.hashCode) && (Hash.fromUniversalHashCode[A].hash(x) == x.hashCode()) &&
      (E.eqv(x, y) == Hash.fromUniversalHashCode[A].eqv(x, y))) <-> true

  @deprecated("This law is no longer enforced", "2.9.0")
  def sameAsScalaHashing(x: A, y: A, scalaHashing: Hashing[A]): IsEq[Boolean] =
    ((E.hash(x) == Hash.fromHashing(scalaHashing).hash(x)) &&
      (E.eqv(x, y) == Hash.fromHashing(scalaHashing).eqv(x, y))) <-> true

}

object HashLaws {
  def apply[A](implicit ev: Hash[A]): HashLaws[A] =
    new HashLaws[A] { def E: Hash[A] = ev }
}
