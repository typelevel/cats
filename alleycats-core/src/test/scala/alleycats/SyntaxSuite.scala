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

package alleycats

import cats.{Eq, Foldable}
import alleycats.syntax.all.{catsSyntaxExtract, EmptyOps, ExtraFoldableOps}

import scala.collection.immutable.SortedMap

/**
 * Test that our syntax implicits are working.
 *
 * Each method should correspond to one type class worth of syntax.
 * Ideally, we should be testing every operator or method that we
 * expect to add to generic parameters. This file is a safeguard
 * against accidentally breaking (or removing) syntax which was
 * otherwise untested.
 *
 * The strategy here is to create "mock" values of particular types,
 * and then ensure that the syntax we want is available. We never plan
 * to run any of these methods, so we don't need real values. All
 * values in the methods should be generic -- we rely on parametricity
 * to guarantee that the syntax will be available for any type with
 * the proper type class instance(s).
 *
 * None of these tests should ever run, or do any runtime checks.
 */
object SyntaxSuite {

  // pretend we have a value of type A
  def mock[A]: A = ???

  def testEmpty[A: Empty](): Unit = {
    val x = mock[A]
    implicit val y: Eq[A] = mock[Eq[A]]
    val _ = x.isEmpty | x.nonEmpty
  }

  def testOptionEmpty(): Unit = {
    case class A[T](x: T)
    case class B()

    implicit def emptyA[T: Empty]: Empty[A[T]] = new Empty[A[T]] {
      def empty: A[T] = A(Empty[T].empty)
    }

    val _ = Empty[A[Option[B]]].empty
  }

  def testMapEmpty(): Any = {
    case class Foo[A](foo: A)

    val _ = Empty[Map[Foo[Int], Foo[String]]].empty
  }

  def testSortedMapEmpty(): Any = {
    case class Foo[A](foo: A)

    implicit def fooOrd[A: Ordering]: Ordering[Foo[A]] = Ordering.by(_.foo)

    val _ = Empty[SortedMap[Foo[Int], Foo[String]]].empty
  }

  def testFoldable[F[_]: Foldable, A](): Unit = {
    val x = mock[F[A]]
    val y = mock[A => Unit]
    val _ = x.foreach(y)
  }

  def testExtract[F[_]: Extract, A](): Unit = {
    val x = mock[F[A]]
    val _ = x.extract
  }
}
