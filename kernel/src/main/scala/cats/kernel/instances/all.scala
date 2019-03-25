package cats.kernel
package instances

package object all extends AllInstances with AllInstancesBinCompat0

trait AllInstances
    extends BigDecimalInstances
    with BigIntInstances
    with BitSetInstances
    with BooleanInstances
    with ByteInstances
    with CharInstances
    with DoubleInstances
    with EqInstances
    with EitherInstances
    with DurationInstances
    with FloatInstances
    with FunctionInstances
    with HashInstances
    with IntInstances
    with ListInstances
    with ListSetInstances
    with LongInstances
    with MapInstances
    with OptionInstances
    with OrderInstances
    with PartialOrderInstances
    with QueueInstances
    with SetInstances
    with ShortInstances
    with StreamInstances
    with StringInstances
    with SymbolInstances
    with TupleInstances
    with UnitInstances
    with UUIDInstances
    with VectorInstances

trait AllInstancesBinCompat0 extends FiniteDurationInstances
