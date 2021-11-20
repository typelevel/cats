package cats

/**
 * Convenience methods and values for Either.
 */
private[cats] object EitherUtil {
  def leftCast[A, B, C](right: Right[A, B]): Either[C, B] =
    right.asInstanceOf[Either[C, B]]
  def rightCast[A, B, C](left: Left[A, B]): Either[A, C] =
    left.asInstanceOf[Either[A, C]]

  val unit = Right(())
  val leftUnit = Left(())
}
