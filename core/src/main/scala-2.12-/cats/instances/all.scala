package cats
package instances

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

private[cats] trait AllInstancesBinCompat0 extends FunctionInstancesBinCompat0 with Tuple2InstancesBinCompat0

private[cats] trait AllInstancesBinCompat1
    extends OptionInstancesBinCompat0
    with ListInstancesBinCompat0
    with VectorInstancesBinCompat0
    with StreamInstancesBinCompat0
    with MapInstancesBinCompat0
    with SortedMapInstancesBinCompat0

private[cats] trait AllInstancesBinCompat2 extends DurationInstances with FiniteDurationInstances

private[cats] trait AllInstancesBinCompat3 extends AllCoreDurationInstances

private[cats] trait AllInstancesBinCompat4 extends SortedMapInstancesBinCompat1 with MapInstancesBinCompat1

private[cats] trait AllInstancesBinCompat5 extends SortedSetInstancesBinCompat0

private[cats] trait AllInstancesBinCompat6
    extends SortedSetInstancesBinCompat1
    with SortedMapInstancesBinCompat2
    with FunctionInstancesBinCompat1
    with cats.kernel.instances.EitherInstancesBinCompat0
    with cats.kernel.instances.FunctionInstancesBinCompat0
