package cats
package kernel

package compat

object lazyList {
  type LazyList[+A] = scala.LazyList[A] //this is needed only to avoid unused import warnings on Scala 2.13
  val LazyList = scala.LazyList
}
