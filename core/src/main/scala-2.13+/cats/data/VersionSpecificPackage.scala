package cats

package data

private[data] abstract class VersionSpecificPackage {
  type NonEmptyLazyList[+A] = NonEmptyLazyListImpl.Type[A]
  val NonEmptyLazyList = NonEmptyLazyListImpl  
  
  type NonEmptyVector[+A] = NonEmptyVector.Type[A]

}