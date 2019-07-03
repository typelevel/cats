package cats

package data

private[data] abstract class VersionSpecificPackage {
  type NonEmptyLazyList[+A] = NonEmptyLazyList.Type[A]

  type NonEmptyList[+A] = NonEmptyList.Type[A]

  type NonEmptyVector[+A] = NonEmptyVector.Type[A]

}