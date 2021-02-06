package cats

package object instances {
  object all extends AllInstancesBinCompat
  object arraySeq extends ArraySeqInstances
  object bigInt extends BigIntInstances
  object bigDecimal extends BigDecimalInstances
  object bitSet extends BitSetInstances
  object boolean extends BooleanInstances
  object byte extends ByteInstances
  object char extends CharInstances
  object double extends DoubleInstances
  object duration extends CoreDurationInstances with DurationInstances
  object either extends EitherInstances
  object eq extends EqInstances
  object equiv extends EquivInstances
  object float extends FloatInstances
  object finiteDuration extends CoreFiniteDurationInstances with FiniteDurationInstances
  object function extends FunctionInstances with FunctionInstancesBinCompat0
  object partialFunction extends PartialFunctionInstances
  object future extends FutureInstances
  object int extends IntInstances
  object invariant extends InvariantMonoidalInstances with InvariantInstances
  object list extends ListInstances with ListInstancesBinCompat0
  object long extends LongInstances
  object option extends OptionInstances with OptionInstancesBinCompat0
  object map extends MapInstances with MapInstancesBinCompat0 with MapInstancesBinCompat1
  object order extends OrderInstances
  object ordering extends OrderingInstances
  object parallel extends ParallelInstances
  object partialOrder extends PartialOrderInstances
  object partialOrdering extends PartialOrderingInstances
  object queue extends QueueInstances
  object set extends SetInstances
  object seq extends SeqInstances
  object short extends ShortInstances
  object sortedMap
      extends SortedMapInstances
      with SortedMapInstancesBinCompat0
      with SortedMapInstancesBinCompat1
      with SortedMapInstancesBinCompat2
  object sortedSet extends SortedSetInstances with SortedSetInstancesBinCompat0 with SortedSetInstancesBinCompat1

  @deprecated("Use cats.instances.lazyList", "2.0.0-RC2")
  object stream extends StreamInstances with StreamInstancesBinCompat0
  object lazyList extends LazyListInstances
  object string extends StringInstances
  object tailRec extends TailRecInstances
  object try_ extends TryInstances
  object tuple extends TupleInstances with Tuple2InstancesBinCompat0
  object unit extends UnitInstances
  object uuid extends UUIDInstances
  object vector extends VectorInstances with VectorInstancesBinCompat0
}
