package cats.kernel
package laws

import cats.kernel.instances.int._
import cats.kernel.instances.lazyList._
import cats.kernel.instances.option._
import cats.kernel.laws.discipline.{EqTests, HashTests, MonoidTests, OrderTests, PartialOrderTests, SerializableTests}

trait ScalaVersionSpecificTests { this: Tests =>
  checkAll("Eq[LazyList[HasEq[Int]]]", EqTests[LazyList[HasEq[Int]]].eqv)
  checkAll("PartialOrder[LazyList[HasPartialOrder[Int]]]",
           PartialOrderTests[LazyList[HasPartialOrder[Int]]].partialOrder)
  checkAll("Order[LazyList[Int]]", OrderTests[LazyList[Int]].order)
  checkAll("Monoid[LazyList[Int]]", MonoidTests[LazyList[Int]].monoid)
  checkAll("Monoid[LazyList[Int]]", SerializableTests.serializable(Monoid[LazyList[Int]]))
  checkAll("Hash[LazyList[Int]]", HashTests[LazyList[Int]].hash)
}
