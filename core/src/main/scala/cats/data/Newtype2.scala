package cats
package data

/**
  * Helper trait for `newtype`s with two type parameters.
  * These allow you to create a zero-allocation wrapper around a specific type.
  * Similar to `AnyVal` value classes, but never have any runtime overhead.
  */
trait Newtype2 { self =>
  private[data] type Base
  private[data] trait Tag extends Any
  type Type[A, +B] <: Base with Tag
}
