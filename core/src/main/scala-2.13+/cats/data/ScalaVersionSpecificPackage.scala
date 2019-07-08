package cats

package data

abstract private[data] class ScalaVersionSpecificPackage {
  type NonEmptyLazyList[+A] = NonEmptyLazyList.Type[A]

  type NonEmptyList[+A] = NonEmptyList.Type[A]

  type NonEmptyVector[+A] = NonEmptyVector.Type[A]

}
