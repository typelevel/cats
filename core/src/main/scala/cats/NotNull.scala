package cats

/**
 * An instance of `NotNull[A]` indicates that `A` does not have a static type
 * of `Null`.
 *
 * This can be useful in preventing `Null` from being inferred when a type
 * parameter is omitted.
 */
sealed trait NotNull[A]

object NotNull {
  /**
   * Since NotNull is just a marker trait with no functionality, it's safe to
   * reuse a single instance of it. This helps prevent unnecessary allocations.
   */
  private[this] val singleton: NotNull[Any] = new NotNull[Any] {}

  implicit def ambiguousNull1: NotNull[Null] = throw new Exception("An instance of NotNull[Null] was used. This should never happen. Both ambiguousNull1 and ambiguousNull2 should always be in scope if one of them is.")

  implicit def ambiguousNull2: NotNull[Null] = ambiguousNull1

  implicit def notNull[A]: NotNull[A] = singleton.asInstanceOf[NotNull[A]]
}
