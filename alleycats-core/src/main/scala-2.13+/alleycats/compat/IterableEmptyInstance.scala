package alleycats
package compat

import collection.Factory

abstract class IterableEmptyInstance {
  implicit def iterableIsEmpty[CC[X] <: Iterable[X], A](implicit factory: Factory[A, CC[A]]): Empty[CC[A]] =
    Empty(factory.newBuilder.result())
}
