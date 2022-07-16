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

package object evidence {

  /**
   * A convenient type alias for Is, which declares that A is the same
   * type as B.
   */
  type ===[A, B] = A Is B

  /**
   * This type level equality represented by `Is` is referred to as
   * "Leibniz equality", and it had the name "Leibniz" in the scalaz
   *  https://en.wikipedia.org/wiki/Gottfried_Wilhelm_Leibniz
   */
  type Leibniz[A, B] = A Is B

  /**
   * A convenient type alias for As, this declares that A is a
   * subtype of B, and should be able to be  a B is
   * expected.
   */
  type <~<[-A, +B] = A As B

  /**
   * A flipped alias, for those used to their arrows running left to right
   */
  type >~>[+B, -A] = A As B

  /**
   * The property that a value of type A can be used in a context
   * expecting a B if A <~< B is referred to as the "Liskov
   * Substitution Principle", which is named for Barbara Liskov:
   * https://en.wikipedia.org/wiki/Barbara_Liskov
   */
  type Liskov[-A, +B] = A As B
}
