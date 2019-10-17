package cats
package instances

abstract class AllInstancesBinCompat
    extends AllInstances
    with AllInstancesBinCompat0
    with AllInstancesBinCompat1
    with AllInstancesBinCompat2
    with AllInstancesBinCompat3
    with AllInstancesBinCompat4
    with AllInstancesBinCompat5
    with AllInstancesBinCompat6

trait AllInstances
    extends AnyValInstances
    with BigIntInstances
    with BigDecimalInstances
    with BitSetInstances
    with EitherInstances
    with EqInstances
    with EquivInstances
    with FunctionInstances
    with FutureInstances
    with HashInstances
    with InvariantMonoidalInstances
    with ListInstances
    with MapInstances
    with OptionInstances
    with OrderInstances
    with OrderingInstances
    with ParallelInstances
    with PartialOrderInstances
    with PartialOrderingInstances
    with QueueInstances
    with SetInstances
    with SortedMapInstances
    with SortedSetInstances
    with StreamInstances
    with StringInstances
    with SymbolInstances
    with TryInstances
    with TupleInstances
    with UUIDInstances
    with VectorInstances

trait AllInstancesBinCompat0 extends FunctionInstancesBinCompat0 with Tuple2InstancesBinCompat0

trait AllInstancesBinCompat1
    extends OptionInstancesBinCompat0
    with ListInstancesBinCompat0
    with VectorInstancesBinCompat0
    with StreamInstancesBinCompat0
    with MapInstancesBinCompat0
    with SortedMapInstancesBinCompat0

trait AllInstancesBinCompat2 extends DurationInstances with FiniteDurationInstances

trait AllInstancesBinCompat3 extends AllCoreDurationInstances

trait AllInstancesBinCompat4 extends SortedMapInstancesBinCompat1 with MapInstancesBinCompat1

trait AllInstancesBinCompat5 extends SortedSetInstancesBinCompat0

trait AllInstancesBinCompat6 extends SortedSetInstancesBinCompat1 with SortedMapInstancesBinCompat2
