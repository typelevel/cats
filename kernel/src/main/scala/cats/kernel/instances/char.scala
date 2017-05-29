package cats.kernel
package instances

package object char extends CharInstances

trait CharInstances {
  implicit val catsKernelStdEqForChar: Order[Char] with Hash[Char] = new CharEq
}

class CharEq extends Order[Char] with Hash[Char] {
  def hash(x: Char): Int = x.##
  def compare(x: Char, y: Char): Int =
    if (x < y) -1 else if (x > y) 1 else 0
  override def eqv(x:Char, y:Char): Boolean = x == y
  override def neqv(x:Char, y:Char): Boolean = x != y
  override def gt(x: Char, y: Char): Boolean = x > y
  override def gteqv(x: Char, y: Char): Boolean = x >= y
  override def lt(x: Char, y: Char): Boolean = x < y
  override def lteqv(x: Char, y: Char): Boolean = x <= y
}
