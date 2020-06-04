package cats
package kernel

import scala.{specialized => sp}

trait PartialNext[@sp A] extends UpperBounded[A] {
  def partialOrder: PartialOrder[A]
  def partialNext(a: A): Option[A]
}

trait Next[@sp A] {
  def partialOrder: PartialOrder[A]
  def next(a: A): A
}

trait PartialPrevious[@sp A] extends LowerBounded[A] {
  def partialOrder: PartialOrder[A]
  def partialPrevious(a: A): Option[A]
}

trait Previous[@sp A] {
  def partialOrder: PartialOrder[A]
  def previous(a: A): A
}

trait UnboundedEnum[@sp A] extends Next[A] with Previous[A] {
  def order: Order[A]
}

trait BoundedEnum[@sp A] extends PartialNext[A] with PartialPrevious[A] {
  def order: Order[A]
  override final def partialOrder = order
}

trait LowerBoundedEnum[@sp A] extends Next[A] with PartialPrevious[A] {
  def order: Order[A]
  override final def partialOrder = order
}

trait UpperBoundedEnum[@sp A] extends PartialNext[A] with Previous[A] {
  def order: Order[A]
  override final def partialOrder = order
}
