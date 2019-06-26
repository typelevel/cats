package cats.kernel.compat
import scala.annotation.{Annotation, StaticAnnotation}

private[cats] object scalaVersionMoreSpecific {
  /**
   * a trick to suppress unused import warning for this object
   */
  class suppressUnusedImportWarningForScalaVersionMoreSpecific extends Annotation with StaticAnnotation
}
