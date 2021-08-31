package alleycats
package compat
import scala.annotation.{Annotation, StaticAnnotation}

private[alleycats] object scalaVersionSpecific {

  /**
   * a trick to suppress unused import warning for this object
   */
  class suppressUnusedImportWarningForScalaVersionSpecific extends Annotation with StaticAnnotation

}
