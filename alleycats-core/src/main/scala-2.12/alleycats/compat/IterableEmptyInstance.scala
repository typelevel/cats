package alleycats
package compat

import scala.collection.generic.CanBuildFrom

abstract class IterableEmptyInstance {
  implicit def iterableIsEmpty[CC[X] <: Iterable[X], A](implicit cbf: CanBuildFrom[CC[A], A, CC[A]]): Empty[CC[A]] =
    Empty(cbf().result)
}
