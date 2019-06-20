package cats
package kernel

package object compat {
  type Stream[+A] = scala.LazyList[A]

  val Stream = LazyList
}
