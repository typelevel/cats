package cats
package data

trait Newtype2 { self =>
  private[data] type Base
  private[data] trait Tag extends Any
  private[cats] type Type[A, +B] <: Base with Tag
}
