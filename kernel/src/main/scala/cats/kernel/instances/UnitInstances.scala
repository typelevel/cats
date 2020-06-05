package cats.kernel
package instances
import compat.scalaVersionSpecific._

@suppressUnusedImportWarningForScalaVersionSpecific
trait UnitInstances {
  implicit val catsKernelStdOrderForUnit: Order[Unit] with Hash[Unit] with BoundedEnum[Unit] =
    new UnitOrder

  implicit val catsKernelStdAlgebraForUnit: BoundedSemilattice[Unit] with CommutativeGroup[Unit] =
    new UnitAlgebra
}

trait UnitBounded extends BoundedEnum[Unit] {
  override def minBound: Unit = ()
  override def maxBound: Unit = ()
  override def partialNext(x: Unit): Option[Unit] = None
  override def partialPrevious(x: Unit): Option[Unit] = None
}

class UnitOrder extends Order[Unit] with Hash[Unit] with UnitBounded { self =>
  def compare(x: Unit, y: Unit): Int = 0

  def hash(x: Unit): Int = 0 // ().hashCode() == 0

  override def eqv(x: Unit, y: Unit): Boolean = true
  override def neqv(x: Unit, y: Unit): Boolean = false
  override def gt(x: Unit, y: Unit): Boolean = false
  override def lt(x: Unit, y: Unit): Boolean = false
  override def gteqv(x: Unit, y: Unit): Boolean = true
  override def lteqv(x: Unit, y: Unit): Boolean = true

  override def min(x: Unit, y: Unit): Unit = ()
  override def max(x: Unit, y: Unit): Unit = ()

  override val order: Order[Unit] = self
}

class UnitAlgebra extends BoundedSemilattice[Unit] with CommutativeGroup[Unit] {
  def empty: Unit = ()
  def combine(x: Unit, y: Unit): Unit = ()
  override def remove(x: Unit, y: Unit): Unit = ()
  def inverse(x: Unit): Unit = ()
  override protected[this] def repeatedCombineN(a: Unit, n: Int): Unit = ()
  override def combineAllOption(as: IterableOnce[Unit]): Option[Unit] =
    if (as.iterator.isEmpty) None else Some(())
}
