package cats
package kernel

package compat
import scala.annotation.{Annotation, StaticAnnotation}

private[cats] object scalaVersionSpecific {

  /**
   * a trick to suppress unused import warning for this object
   */
  class suppressUnusedImportWarningForScalaVersionSpecific extends Annotation with StaticAnnotation

  type IterableOnce[+A] = scala.IterableOnce[A] //this is needed only to avoid unused import warnings on Scala 2.13

  implicit class iterableOnceExtension[A](private val io: IterableOnce[A]) extends AnyVal {
    def reduceOption(f: (A, A) => A): Option[A] = io.iterator.reduceOption(f)
  }
}
