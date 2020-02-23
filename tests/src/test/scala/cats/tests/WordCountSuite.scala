package cats
package tests

import cats.data.{AppFunc, Const, Func}
import Func.appFunc

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
      appFunc((c: Char) => liftInt(testIf(c == '\n')))
    def isSpace(c: Char): Boolean = (c == ' ' || c == '\n')

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
    charCount.getConst should ===(96)
    lineCount.getConst should ===(2)
    wordCount.getConst should ===(17)
  }
}
