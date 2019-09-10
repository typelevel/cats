package cats.kernel.compat
import scala.annotation.{Annotation, StaticAnnotation}

private[cats] object scalaVersionMoreSpecific {
  implicit def toEitherExtensionOps[A, B](either: Either[A, B]): EitherExtensionOps[A, B] =
    new EitherExtensionOps(either)

  class EitherExtensionOps[A, B](val either: Either[A, B]) extends AnyVal {
    @inline def toOption: Option[B] = either.right.toOption
  }

  /**
   * a trick to suppress unused import warning for this object
   */
  class suppressUnusedImportWarningForScalaVersionMoreSpecific extends Annotation with StaticAnnotation
}
