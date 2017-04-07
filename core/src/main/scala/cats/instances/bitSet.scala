package cats.instances

import scala.collection.immutable.BitSet
import int.catsStdShowForInt
import cats.syntax.show._
import cats.Show

trait BitSetInstances extends cats.kernel.instances.BitSetInstances {
  implicit def catsStdShowForBitSet: Show[BitSet] = new Show[BitSet] {
    def show(fa: BitSet): String =
      fa.toIterator.map(_.show).mkString("BitSet(", ", ", ")")
  }
}
