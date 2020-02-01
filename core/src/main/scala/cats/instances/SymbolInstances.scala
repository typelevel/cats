package cats.instances

import cats.Show

trait SymbolInstances extends cats.kernel.instances.SymbolInstances {
  implicit val catsStdShowForSymbol: Show[Symbol] =
    Show.fromToString[Symbol]
}
