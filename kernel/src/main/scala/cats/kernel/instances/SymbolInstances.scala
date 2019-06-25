package cats.kernel
package instances

trait SymbolInstances {
  implicit val catsKernelStdOrderForSymbol: Order[Symbol] with Hash[Symbol] with LowerBounded[Symbol] = new SymbolOrder
}

trait SymbolLowerBounded extends LowerBounded[Symbol] {
  override def minBound: Symbol = Symbol("")
}

class SymbolOrder extends Order[Symbol] with Hash[Symbol] with SymbolLowerBounded {

  def hash(x: Symbol): Int = x.hashCode()

  override def eqv(x: Symbol, y: Symbol): Boolean =
    // Symbols are interned
    x eq y

  def compare(x: Symbol, y: Symbol): Int =
    if (x eq y) 0 else x.name.compareTo(y.name)

  override val partialOrder: PartialOrder[Symbol] = this
}
