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

import cats.{Eval, Foldable, Order, Reducible, Traverse, UnorderedFoldable}
import cats.data.NonEmptyList
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}

// Each object has its own syntax imports, without an enclosing cats.syntax.all import.
private[tests] object UnorderedFoldableMinimumMaximumSyntax {
  object UnorderedOnly {
    import cats.syntax.unorderedFoldable.*
    def apply[F[_]: UnorderedFoldable, A: Order](fa: F[A]): (Option[A], Option[A]) =
      (fa.minimumOption, fa.maximumOption)
  }

  object FoldableOnly {
    import cats.syntax.foldable.*
    def apply[F[_]: Foldable, A: Order](fa: F[A]): (Option[A], Option[A]) =
      (fa.minimumOption, fa.maximumOption)
  }

  object Combined {
    import cats.syntax.unorderedFoldable.*
    import cats.syntax.foldable.*
    def foldable[F[_]: Foldable, A: Order](fa: F[A]): (Option[A], Option[A]) =
      (fa.minimumOption, fa.maximumOption)
  }

  object CombinedUnordered {
    import cats.syntax.unorderedFoldable.*
    // Both wildcard imports export this same inherited conversion, hiding it on Scala 2.
    import cats.syntax.foldable.{toUnorderedFoldableOps => _, *}
    def apply[F[_]: UnorderedFoldable, A: Order](fa: F[A]): (Option[A], Option[A]) =
      (fa.minimumOption, fa.maximumOption)
  }

  object All {
    import cats.syntax.all.*
    def unordered[F[_]: UnorderedFoldable, A: Order](fa: F[A]): (Option[A], Option[A]) =
      (fa.minimumOption, fa.maximumOption)
    def foldable[F[_]: Foldable, A: Order](fa: F[A]): (Option[A], Option[A]) =
      (fa.minimumOption, fa.maximumOption)
    def traverse[F[_]: Traverse, A: Order](fa: F[A]): (Option[A], Option[A]) =
      (fa.minimumOption, fa.maximumOption)
    def reducible[F[_]: Reducible, A: Order](fa: F[A]): (Option[A], Option[A]) =
      (fa.minimumOption, fa.maximumOption)
  }
}

class UnorderedFoldableSyntaxSuite extends CatsSuite {
  import UnorderedFoldableMinimumMaximumSyntax.*

  private val expected = (Some(1), Some(3))

  test("minimumOption/maximumOption with only unorderedFoldable syntax") {
    assertEquals(UnorderedOnly(Set(3, 1, 2)), expected)
    assertEquals(UnorderedOnly(Map("a" -> 3, "b" -> 1, "c" -> 2)), expected)
    assertEquals(UnorderedOnly(List(3, 1, 2)), expected)
    assertEquals(UnorderedOnly(Set.empty[Int]), (None, None))
    assertEquals(UnorderedOnly(Set(2)), (Some(2), Some(2)))
  }

  test("minimumOption/maximumOption with only foldable syntax") {
    assertEquals(FoldableOnly(List(3, 1, 2)), expected)
  }

  test("minimumOption/maximumOption with both syntax imports") {
    assertEquals(CombinedUnordered(Set(3, 1, 2)), expected)
    assertEquals(Combined.foldable(List(3, 1, 2)), expected)
  }

  test("minimumOption/maximumOption with all syntax and stronger typeclasses") {
    assertEquals(All.unordered(Set(3, 1, 2)), expected)
    assertEquals(All.foldable(List(3, 1, 2)), expected)
    assertEquals(All.traverse(List(3, 1, 2)), expected)
    assertEquals(All.reducible(NonEmptyList.of(3, 1, 2)), expected)
  }

  test("all syntax paths dispatch to Foldable minimumOption/maximumOption overrides") {
    var minimumCalls = 0
    var maximumCalls = 0
    implicit val F: Foldable[List] = new Foldable[List] {
      def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
      def foldRight[A, B](fa: List[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        cats.instances.list.catsStdInstancesForList.foldRight(fa, b)(f)
      override def minimumOption[A](fa: List[A])(implicit A: Order[A]): Option[A] = {
        minimumCalls += 1
        super.minimumOption(fa)
      }
      override def maximumOption[A](fa: List[A])(implicit A: Order[A]): Option[A] = {
        maximumCalls += 1
        super.maximumOption(fa)
      }
      override def unorderedReduceOption[A: CommutativeSemigroup](fa: List[A]): Option[A] =
        throw new IllegalStateException("Foldable extrema must use the ordered implementation")
    }
    val values = List(3, 1, 2)
    assertEquals(UnorderedOnly(values), expected)
    assertEquals(FoldableOnly(values), expected)
    assertEquals(CombinedUnordered(values), expected)
    assertEquals(Combined.foldable(values), expected)
    assertEquals(All.unordered(values), expected)
    assertEquals(All.foldable(values), expected)
    assertEquals(minimumCalls, 6)
    assertEquals(maximumCalls, 6)
  }

  test("unordered extrema are invariant under permutations up to Order equality") {
    final case class Entry(key: Int, label: String)
    val order: Order[Entry] = Order.by(_.key)
    val F = new UnorderedFoldable[List] {
      def unorderedFoldMap[A, B: CommutativeMonoid](fa: List[A])(f: A => B): B = {
        val B = CommutativeMonoid[B]
        fa.foldLeft(B.empty)((b, a) => B.combine(b, f(a)))
      }
    }
    val values =
      List(Entry(1, "first minimum"), Entry(2, "first maximum"), Entry(1, "second minimum"), Entry(2, "second maximum"))
    values.permutations.foreach { permutation =>
      val minimum = F.minimumOption(permutation)(using order).get
      val maximum = F.maximumOption(permutation)(using order).get
      assert(values.contains(minimum))
      assert(values.contains(maximum))
      assert(order.eqv(minimum, values.head))
      assert(order.eqv(maximum, values(1)))
    }
  }
}
