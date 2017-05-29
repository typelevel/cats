package cats.kernel
package instances

package object symbol extends SymbolInstances

trait SymbolInstances {
  implicit val catsKernelStdEqForSymbol: Order[Symbol] with Hash[Symbol] = new SymbolEq
}

class SymbolEq extends Order[Symbol] with Hash[Symbol] {

  def hash(x: Symbol): Int = x.##

  override def eqv(x: Symbol, y: Symbol): Boolean = {
    // Symbols are interned
    x eq y
  }

  def compare(x: Symbol, y: Symbol): Int =
    if (x eq y) 0 else x.name compareTo y.name
}
