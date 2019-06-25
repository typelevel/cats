package cats.kernel
package instances

trait SymbolInstances {
  implicit val catsKernelStdOrderForSymbol: Order[Symbol] with Hash[Symbol] = new SymbolOrder
  implicit val catsKernelStdLowerBoundedForSymbol: LowerBounded[Symbol] =
    new LowerBounded[Symbol] {
      override def minBound: Symbol = Symbol("")
      override val partialOrder: PartialOrder[Symbol] = catsKernelStdOrderForSymbol
    }
}

class SymbolOrder extends Order[Symbol] with Hash[Symbol] {

  def hash(x: Symbol): Int = x.hashCode()

  override def eqv(x: Symbol, y: Symbol): Boolean =
    // Symbols are interned
    x eq y

  def compare(x: Symbol, y: Symbol): Int =
    if (x eq y) 0 else x.name.compareTo(y.name)
}
