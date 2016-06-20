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

  private[this] def ambiguousException: Exception = new Exception("An instance of NotNull[Null] was used. This should never happen. Both ambiguous NotNull[Null] instances should always be in scope if one of them is.")

  implicit def `If you are seeing this, you probably need to add an explicit type parameter somewhere, because Null is being inferred.`: NotNull[Null] = throw ambiguousException

  implicit def catsAmbiguousNotNullNull2: NotNull[Null] = throw ambiguousException

  implicit def catsNotNullForA[A]: NotNull[A] = singleton.asInstanceOf[NotNull[A]]
}
