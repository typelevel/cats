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

package cats.bench

import org.openjdk.jmh.annotations.*
import cats.{CommutativeApplicative, Eval, UnorderedTraverse}
import java.util.concurrent.TimeUnit
import cats.data.Chain

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class UnorderedTraverseMapBench {
  // These benchmarks were written to choose the fastest implementation
  // for Map.unorderedTraverse, some results are available here:
  // https://github.com/typelevel/cats/pull/4463#issuecomment-1599612154

  val instance = UnorderedTraverse[Map[Int, *]]

  val xs1: Map[Int, Int] = (1 to 1000).map(x => (x, x)).toMap
  val xs2: Map[Int, Int] = (1 to 1000000).map(x => (x, x)).toMap

  def unorderedTraverseViaTree[G[_], A, B](
    fa: Map[Int, A]
  )(f: A => G[B])(implicit G: CommutativeApplicative[G]): G[Map[Int, B]] = {
    def runHalf(size: Int, fa: Map[Int, A]): Eval[G[Map[Int, B]]] =
      if (size > 1) {
        val leftSize = size / 2
        val rightSize = size - leftSize
        val (leftL, rightL) = fa.splitAt(leftSize)
        runHalf(leftSize, leftL).flatMap { left =>
          val right = runHalf(rightSize, rightL)
          G.map2Eval(left, right) { (lm, rm) => lm ++ rm }
        }
      } else {
        val (k, a) = fa.head
        Eval.always(G.map(f(a))(b => Map(k -> b)))
      }

    val len = fa.size
    if (len == 0) G.pure(Map.empty)
    else runHalf(len, fa).value
  }

  def unorderedTraverseViaChain[G[_], A, B](
    fa: Map[Int, A]
  )(f: A => G[B])(implicit G: CommutativeApplicative[G]): G[Map[Int, B]] = {
    if (fa.isEmpty) G.pure(Map.empty[Int, B])
    else
      G.map(Chain.traverseViaChain(fa.toIndexedSeq) { case (k, a) =>
        G.map(f(a))((k, _))
      })(_.iterator.toMap)
  }

  @Benchmark def unorderedTraverseTupleViaTree1: (Int, Map[Int, Int]) =
    unorderedTraverseViaTree(xs1)(x => (x, x))
  @Benchmark def unorderedTraverseTupleViaChain1: (Int, Map[Int, Int]) =
    unorderedTraverseViaChain(xs1)(x => (x, x))

  @Benchmark def unorderedTraverseTupleViaTree2: (Int, Map[Int, Int]) =
    unorderedTraverseViaTree(xs2)(x => (x, x))
  @Benchmark def unorderedTraverseTupleViaChain2: (Int, Map[Int, Int]) =
    unorderedTraverseViaChain(xs2)(x => (x, x))

}
