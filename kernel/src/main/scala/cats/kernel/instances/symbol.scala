package cats.kernel
package instances

package object symbol extends SymbolInstances

trait SymbolInstances {
  implicit val catsKernelStdOrderForSymbol: Order[Symbol] = new SymbolOrder
}

class SymbolOrder extends Order[Symbol] {

  override def eqv(x: Symbol, y: Symbol): Boolean = {
    // Symbols are interned
    x eq y
  }

  def compare(x: Symbol, y: Symbol): Int =
    if (x eq y) 0 else x.name compareTo y.name
}
