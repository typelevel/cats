package cats.kernel
package instances

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

trait AllInstancesBinCompat1
    extends EitherInstancesBinCompat0
    with ListInstancesBinCompat0
    with OptionInstancesBinCompat0
    with QueueInstancesBinCompat0
    with SetInstancesBinCompat0
    with StreamInstancesBinCompat0
    with TupleInstancesBinCompat0
    with UUIDInstancesBinCompat0
    with VectorInstancesBinCompat0
