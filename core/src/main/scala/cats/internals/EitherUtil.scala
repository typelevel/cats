package cats
package internals

/**
 * Internal API — Convenience functions for `Either`.
 */
private[cats] object EitherUtil {
  @inline def leftCast[A, B, C](right: Right[A, B]): Either[C, B] =
    right.asInstanceOf[Either[C, B]]

  @inline def rightCast[A, B, C](left: Left[A, B]): Either[A, C] =
    left.asInstanceOf[Either[A, C]]

  @inline def eitherCast[A, B](either: Either[_, _]): Either[A, B] =
    either.asInstanceOf[Either[A, B]]

  /**
   * Internal API — reusable function for boxing values in `Right(_)`.
   * To be used with `andThen`, e.g. `fa.map(f.andThen(rightBox))`.
   */
  def rightBox[A, B]: B => Either[A, B] =
    rightBoxRef.asInstanceOf[B => Either[A, B]]

  /**
   * Internal API — reusable function for boxing values in `Left(_)`.
   * To be used with `andThen`, e.g. `fa.map(f.andThen(leftBox))`.
   */
  def leftBox[A, B]: A => Either[A, B] =
    leftBoxRef.asInstanceOf[A => Either[A, B]]

  private[this] val rightBoxRef: Any => Either[Nothing, Nothing] =
    a => new Right(a).asInstanceOf[Either[Nothing, Nothing]]
  private[this] val leftBoxRef: Any => Either[Nothing, Nothing] =
    a => new Left(a).asInstanceOf[Either[Nothing, Nothing]]
}
