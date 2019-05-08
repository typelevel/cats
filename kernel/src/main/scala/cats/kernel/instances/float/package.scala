package cats.kernel
package instances

package object float extends FloatInstances

trait FloatInstances {
  implicit val catsKernelStdOrderForFloat: Order[Float] with Hash[Float] = new FloatOrder
  implicit val catsKernelStdGroupForFloat: CommutativeGroup[Float] = new FloatGroup
}

/**
 * This is only approximately associative.
 */
class FloatGroup extends CommutativeGroup[Float] {
  def combine(x: Float, y: Float): Float = x + y
  def empty: Float = 0F
  def inverse(x: Float): Float = -x
  override def remove(x: Float, y: Float): Float = x - y
}

/**
 * Due to the way floating-point equality works, this instance is not
 * lawful under equality, but is correct when taken as an
 * approximation of an exact value.
 *
 * If you would prefer an absolutely lawful fractional value, you'll
 * need to investigate rational numbers or more exotic types.
 */
class FloatOrder extends Order[Float] with Hash[Float] {

  def hash(x: Float): Int = x.hashCode()

  def compare(x: Float, y: Float): Int =
    java.lang.Float.compare(x, y)

  override def eqv(x: Float, y: Float): Boolean = x == y
  override def neqv(x: Float, y: Float): Boolean = x != y
  override def gt(x: Float, y: Float): Boolean = x > y
  override def gteqv(x: Float, y: Float): Boolean = x >= y
  override def lt(x: Float, y: Float): Boolean = x < y
  override def lteqv(x: Float, y: Float): Boolean = x <= y

  override def min(x: Float, y: Float): Float =
    java.lang.Math.min(x, y)
  override def max(x: Float, y: Float): Float =
    java.lang.Math.max(x, y)
}
