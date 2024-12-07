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

import cats.data.{AppFunc, Const}
import cats.data.Func.appFunc
import cats.syntax.eq.*

/*
 * This an example of applicative function composition.
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 */
class WordCountSuite extends CatsSuite {
  test("wordcount") {
    import cats.data.State.{get, set}
    val text =
      "Faith, I must leave thee, love, and shortly too.\nMy operant powers their functions leave to do.\n".toList
    // A type alias to treat Int as semigroupal applicative
    type Count[A] = Const[Int, A]
    // Tye type parameter to Count is ceremonial, so hardcode it to Unit
    def liftInt(i: Int): Count[Unit] = Const(i)
    // A simple counter
    def count[A](a: A): Count[Unit] = liftInt(1)

    // An applicative functor to count each character
    val countChar: AppFunc[Count, Char, Unit] = appFunc(count)
    def testIf(b: Boolean): Int = if (b) 1 else 0
    // An applicative functor to count each line
    val countLine: AppFunc[Count, Char, Unit] =
      appFunc { (c: Char) =>
        liftInt(testIf(c == '\n'))
      }
    def isSpace(c: Char): Boolean = c == ' ' || c == '\n'

    // To count words, we need to detect transitions from whitespace to non-whitespace.
    val countWord =
      appFunc { (c: Char) =>
        for {
          x <- get[Boolean]
          y = !isSpace(c)
          _ <- set(y)
        } yield testIf(y && !x)
      }.andThen(appFunc(liftInt))

    val countAll = countWord.product(countLine).product(countChar)
    // Run all applicative functions at once
    val allResults = countAll.traverse(text)
    val wordCountState = allResults.first.first
    val lineCount = allResults.first.second
    val charCount = allResults.second
    val wordCount = wordCountState.value.runA(false).value
    assert(charCount.getConst === 96)
    assert(lineCount.getConst === 2)
    assert(wordCount.getConst === 17)
  }
}
