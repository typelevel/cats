package cats.kernel
package instances

trait UnitInstances {
  implicit val catsKernelStdOrderForUnit: Order[Unit] with Hash[Unit] =
    new UnitOrder

  implicit val catsKernelStdAlgebraForUnit: BoundedSemilattice[Unit] with CommutativeGroup[Unit] =
    new UnitAlgebra

  implicit val catsKernelStdBoundedForUnit: LowerBounded[Unit] with UpperBounded[Unit] =
    new UnitBounded {
      override val partialOrder: PartialOrder[Unit] = catsKernelStdOrderForUnit
    }
}

class UnitOrder extends Order[Unit] with Hash[Unit] {
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
}

class UnitAlgebra extends BoundedSemilattice[Unit] with CommutativeGroup[Unit] {
  def empty: Unit = ()
  def combine(x: Unit, y: Unit): Unit = ()
  override def remove(x: Unit, y: Unit): Unit = ()
  def inverse(x: Unit): Unit = ()
  override protected[this] def repeatedCombineN(a: Unit, n: Int): Unit = ()
  override def combineAllOption(as: TraversableOnce[Unit]): Option[Unit] =
    if (as.isEmpty) None else Some(())
}

trait UnitBounded extends LowerBounded[Unit] with UpperBounded[Unit] {
  override def minBound: Unit = ()
  override def maxBound: Unit = ()
}
