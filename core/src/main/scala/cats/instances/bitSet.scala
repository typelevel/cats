package cats.instances

import scala.collection.immutable.BitSet
import cats.Show

trait BitSetInstances extends cats.kernel.instances.BitSetInstances {
  implicit def catsStdShowForBitSet: Show[BitSet] = Show.fromToString[BitSet]
}
