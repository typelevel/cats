package cats.kernel
package std

package object char extends CharInstances

trait CharInstances {
  implicit val catsKernelStdOrderForChar = new CharOrder
}

class CharOrder extends Order[Char] {
  def compare(x: Char, y: Char) =
    if (x < y) -1 else if (x > y) 1 else 0
  override def eqv(x:Char, y:Char) = x == y
  override def neqv(x:Char, y:Char) = x != y
  override def gt(x: Char, y: Char) = x > y
  override def gteqv(x: Char, y: Char) = x >= y
  override def lt(x: Char, y: Char) = x < y
  override def lteqv(x: Char, y: Char) = x <= y
}
