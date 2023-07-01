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

/**
 * An `ExhuastiveCheck[A]` instance can be used similarly to a ScalaCheck
 * `Gen[A]` instance, but differs in that it generates a `List` of the entire
 * domain of values as opposed to generating a random sampling of values.
 */
trait ExhaustiveCheck[A] extends Serializable { self =>
  def allValues: List[A]
}

object ExhaustiveCheck {
  def apply[A](implicit A: ExhaustiveCheck[A]): ExhaustiveCheck[A] = A

  def instance[A](values: List[A]): ExhaustiveCheck[A] =
    new ExhaustiveCheck[A] {
      val allValues: List[A] = values
    }

  implicit val catsLawsExhaustiveCheckForBoolean: ExhaustiveCheck[Boolean] =
    instance(List(false, true))

  implicit val catsLawsExhaustiveCheckForSetBoolean: ExhaustiveCheck[Set[Boolean]] =
    forSet[Boolean]

  /**
   * Warning: the domain of (A, B) is the cross-product of the domain of `A` and the domain of `B`.
   */
  implicit def catsLawsExhaustiveCheckForTuple2[A, B](implicit
    A: ExhaustiveCheck[A],
    B: ExhaustiveCheck[B]
  ): ExhaustiveCheck[(A, B)] =
    instance(A.allValues.flatMap(a => B.allValues.map(b => (a, b))))

  /**
   * Warning: the domain of (A, B, C) is the cross-product of the 3 domains.
   */
  implicit def catsLawsExhaustiveCheckForTuple3[A, B, C](implicit
    A: ExhaustiveCheck[A],
    B: ExhaustiveCheck[B],
    C: ExhaustiveCheck[C]
  ): ExhaustiveCheck[(A, B, C)] =
    instance(
      for {
        a <- A.allValues
        b <- B.allValues
        c <- C.allValues
      } yield (a, b, c)
    )

  implicit def catsLawsExhaustiveCheckForEither[A, B](implicit
    A: ExhaustiveCheck[A],
    B: ExhaustiveCheck[B]
  ): ExhaustiveCheck[Either[A, B]] =
    instance(A.allValues.map(Left(_)) ++ B.allValues.map(Right(_)))

  implicit def catsLawsExhaustiveCheckForOption[A](implicit A: ExhaustiveCheck[A]): ExhaustiveCheck[Option[A]] =
    instance(None :: A.allValues.map(Some(_)))

  /**
   * Creates an `ExhaustiveCheck[Set[A]]` given an `ExhaustiveCheck[A]` by computing the powerset of
   * values. Note that if there are `n` elements in the domain of `A` there will be `2^n` elements
   * in the domain of `Set[A]`, so use this only on small domains.
   */
  def forSet[A](implicit A: ExhaustiveCheck[A]): ExhaustiveCheck[Set[A]] =
    instance(A.allValues.toSet.subsets().toList)
}
