package cats.kernel
package instances

package object int extends IntInstances

trait IntInstances {
  implicit val catsKernelStdOrderForInt: Order[Int] with Hash[Int] = new IntOrder
  implicit val catsKernelStdGroupForInt: CommutativeGroup[Int] = new IntGroup
}

class IntGroup extends CommutativeGroup[Int] {
  def combine(x: Int, y: Int): Int = x + y
  def empty: Int = 0
  def inverse(x: Int): Int = -x
  override def remove(x: Int, y: Int): Int = x - y
}

class IntOrder extends Order[Int] with Hash[Int] {
  def hash(x: Int): Int = x.hashCode()
  def compare(x: Int, y: Int): Int =
    if (x < y) -1 else if (x > y) 1 else 0

  override def eqv(x: Int, y: Int): Boolean = x == y
  override def neqv(x: Int, y: Int): Boolean = x != y
  override def gt(x: Int, y: Int): Boolean = x > y
  override def gteqv(x: Int, y: Int): Boolean = x >= y
  override def lt(x: Int, y: Int): Boolean = x < y
  override def lteqv(x: Int, y: Int): Boolean = x <= y

  override def min(x: Int, y: Int): Int =
    java.lang.Math.min(x, y)
  override def max(x: Int, y: Int): Int =
    java.lang.Math.max(x, y)
}
