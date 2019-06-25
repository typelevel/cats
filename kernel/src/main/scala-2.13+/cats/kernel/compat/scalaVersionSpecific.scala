package cats
package kernel

package compat

private[cats] object scalaVersionSpecific {
  type LazyList[+A] = scala.LazyList[A] //this is needed only to avoid unused import warnings on Scala 2.13
  type IterableOnce[+A] = scala.IterableOnce[A] //this is needed only to avoid unused import warnings on Scala 2.13

  implicit class iterableOnceExtension[A](private val io: IterableOnce[A]) extends AnyVal {
    def reduceOption(f: (A, A) => A): Option[A] = io.iterator.reduceOption(f)
  }
}
