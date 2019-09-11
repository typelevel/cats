package cats.bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.map._
import cats.instances.list._
import cats.instances.int._
import cats.instances.map._

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
    maps.foldLeft(Map.empty[String, Int]) {
      case (acc, m) => cats.Monoid[Map[String, Int]].combine(acc, m)
    }

  @Benchmark def combineScalaz: Map[String, Int] =
    maps.foldLeft(Map.empty[String, Int]) {
      case (acc, m) => scalaz.Monoid[Map[String, Int]].append(acc, m)
    }

  @Benchmark def combineDirect: Map[String, Int] =
    maps.foldLeft(Map.empty[String, Int]) {
      case (acc, m) => m.foldLeft(acc) {
        case (m, (k, v)) => m.updated(k, v + m.getOrElse(k, 0))
      }
    }

  @Benchmark def combineGeneric: Map[String, Int] =
    combineMapsGeneric[String, Int](maps, 0, _ + _)

  def combineMapsGeneric[K, V](maps: List[Map[K, V]], z: V, f: (V, V) => V): Map[K, V] =
    maps.foldLeft(Map.empty[K, V]) {
      case (acc, m) => m.foldLeft(acc) {
        case (m, (k, v)) => m.updated(k, f(v, m.getOrElse(k, z)))
      }
    }

  @Benchmark def foldMapCats: Map[String, Int] =
    cats.Foldable[List].foldMap(maps)(identity)

  @Benchmark def foldMapScalaz: Map[String, Int] =
    scalaz.Foldable[List].foldMap(maps)(identity)
}
