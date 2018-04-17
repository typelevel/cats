package cats
package instances

abstract class AllInstancesBinCompat
  extends AllInstances
  with AllInstancesBinCompat0

trait AllInstances
  extends AnyValInstances
  with    BigIntInstances
  with    BigDecimalInstances
  with    BitSetInstances
  with    EitherInstances
  with    EqInstances
  with    EquivInstances
  with    FunctionInstances
  with    FutureInstances
  with    HashInstances
  with    InvariantMonoidalInstances
  with    ListInstances
  with    MapInstances
  with    OptionInstances
  with    OrderInstances
  with    OrderingInstances
  with    ParallelInstances
  with    PartialOrderInstances
  with    PartialOrderingInstances
  with    QueueInstances
  with    SetInstances
  with    SortedMapInstances
  with    SortedSetInstances
  with    StreamInstances
  with    StringInstances
  with    SymbolInstances
  with    TryInstances
  with    TupleInstances
  with    UUIDInstances
  with    VectorInstances

trait AllInstancesBinCompat0
  extends OptionInstancesExtension
  with EitherInstancesExtension
