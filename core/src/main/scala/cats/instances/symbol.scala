package cats
package instances

package object symbol extends SymbolInstances

trait SymbolInstances extends cats.kernel.instances.SymbolInstances {
  implicit val catsStdShowForSymbol: Show[Symbol] =
    Show.fromToString[Symbol]
}
