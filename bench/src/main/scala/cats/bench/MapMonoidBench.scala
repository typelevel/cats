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

import cats.instances.list._
import cats.instances.int._
import cats.instances.map._

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class MapMonoidBench {

  val words: List[String] =
    for {
      c <- List("a", "b", "c", "d", "e")
      t <- 1 to 100
    } yield c * t

  val maps: List[Map[String, Int]] = (words ++ words).map(s => Map(s -> 1))

  @Benchmark def combineAllCats: Map[String, Int] =
    cats.Monoid[Map[String, Int]].combineAll(maps)

  @Benchmark def combineCats: Map[String, Int] =
    maps.foldLeft(Map.empty[String, Int]) { case (acc, m) =>
      cats.Monoid[Map[String, Int]].combine(acc, m)
    }

  @Benchmark def combineDirect: Map[String, Int] =
    maps.foldLeft(Map.empty[String, Int]) { case (acc, m) =>
      m.foldLeft(acc) { case (m, (k, v)) =>
        m.updated(k, v + m.getOrElse(k, 0))
      }
    }

  @Benchmark def combineGeneric: Map[String, Int] =
    combineMapsGeneric[String, Int](maps, 0, _ + _)

  def combineMapsGeneric[K, V](maps: List[Map[K, V]], z: V, f: (V, V) => V): Map[K, V] =
    maps.foldLeft(Map.empty[K, V]) { case (acc, m) =>
      m.foldLeft(acc) { case (m, (k, v)) =>
        m.updated(k, f(v, m.getOrElse(k, z)))
      }
    }

  @Benchmark def foldMapCats: Map[String, Int] =
    cats.Foldable[List].foldMap(maps)(identity)
}
