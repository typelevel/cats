package cats

package data

abstract private[data] class ScalaVersionSpecificPackage {
  type NonEmptyLazyList[+A] = NonEmptyLazyList.Type[A]
}
