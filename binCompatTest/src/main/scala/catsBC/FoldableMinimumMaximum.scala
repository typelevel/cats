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

package catsBC

import cats.{Eval, Foldable, Traverse, UnorderedFoldable}
import cats.instances.int.*
import cats.instances.list.*
import cats.kernel.CommutativeMonoid

// Compile these instances and callers against the old cats-core Provided dependency.
object FoldableMinimumMaximum {
  val values: List[Int] = List(3, 1, 2)

  val foldable: Foldable[List] = new Foldable[List] {
    def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
    def foldRight[A, B](fa: List[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      cats.instances.list.catsStdInstancesForList.foldRight(fa, b)(f)
  }

  val unorderedFoldable: UnorderedFoldable[List] = new UnorderedFoldable[List] {
    def unorderedFoldMap[A, B: CommutativeMonoid](fa: List[A])(f: A => B): B = {
      val B = CommutativeMonoid[B]
      fa.foldLeft(B.empty)((b, a) => B.combine(b, f(a)))
    }
  }

  val ops: Foldable.Ops[List, Int] = new Foldable.Ops[List, Int] {
    type TypeClassType = Foldable[List]
    val self: List[Int] = values
    val typeClassInstance: Foldable[List] = foldable
  }

  val allOps: Foldable.AllOps[List, Int] = new Foldable.AllOps[List, Int] {
    type TypeClassType = Foldable[List]
    val self: List[Int] = values
    val typeClassInstance: Foldable[List] = foldable
  }

  val traverseOps: Traverse.AllOps[List, Int] = new Traverse.AllOps[List, Int] {
    type TypeClassType = Traverse[List]
    val self: List[Int] = values
    val typeClassInstance: Traverse[List] = Traverse[List]
  }

  def direct: (Option[Int], Option[Int]) =
    (foldable.minimumOption(values), foldable.maximumOption(values))

  def syntax: (Option[Int], Option[Int]) = {
    import cats.syntax.foldable.*
    implicit val F: Foldable[List] = foldable
    (values.minimumOption, values.maximumOption)
  }

  def throughOps: (Option[Int], Option[Int]) = (ops.minimumOption, ops.maximumOption)
  def throughAllOps: (Option[Int], Option[Int]) = (allOps.minimumOption, allOps.maximumOption)
  def throughTraverseOps: (Option[Int], Option[Int]) = (traverseOps.minimumOption, traverseOps.maximumOption)
}
