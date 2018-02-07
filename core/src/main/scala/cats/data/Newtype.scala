package cats
package data

trait Newtype { self =>
  private[data] type Base
  private[data] trait Tag extends Any
  type Type[A] <: Base with Tag
}
