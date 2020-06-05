package cats
package kernel

import scala.{specialized => sp}

trait PartialNext[@sp A] {
  def partialOrder: PartialOrder[A]
  def partialNext(a: A): Option[A]
}

trait Next[@sp A] extends PartialNext[A] {
  def next(a: A): A
  override def partialNext(a: A): Option[A] = Some(next(a))
}

trait PartialPrevious[@sp A] {
  def partialOrder: PartialOrder[A]
  def partialPrevious(a: A): Option[A]
}

trait Previous[@sp A] extends PartialPrevious[A] {
  def partialOrder: PartialOrder[A]
  def previous(a: A): A
  override def partialPrevious(a: A): Option[A] = Some(previous(a))
}

trait UnboundedEnum[@sp A] extends Next[A] with Previous[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}

trait BoundedEnum[@sp A] extends PartialPrevious[A] with PartialNext[A] with UpperBounded[A] with LowerBounded[A] {

  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order

}

trait LowerBoundedEnum[@sp A] extends Next[A] with PartialPrevious[A] with LowerBounded[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}

trait UpperBoundedEnum[@sp A] extends PartialNext[A] with Previous[A] with UpperBounded[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}
