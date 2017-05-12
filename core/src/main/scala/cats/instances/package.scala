package cats

package object instances {
  object all      extends AllInstances

  object either extends EitherInstances
  object eq extends EqInstances
  object function extends FunctionInstances
  object order extends OrderInstances
  object partialOrder extends PartialOrderInstances
  object monoid extends MonoidInstances
  object semigroup extends SemigroupInstances

  object list       extends ListInstances
  object option     extends OptionInstances
  object set        extends SetInstances
  object bitSet     extends BitSetInstances
  object stream     extends StreamInstances
  object vector     extends VectorInstances
  object map        extends MapInstances
  object future     extends FutureInstances

  object string     extends StringInstances
  object int        extends IntInstances
  object byte       extends ByteInstances
  object long       extends LongInstances
  object char       extends CharInstances
  object short      extends ShortInstances
  object float      extends FloatInstances
  object double     extends DoubleInstances
  object boolean    extends BooleanInstances
  object unit       extends UnitInstances

  object bigInt     extends BigIntInstances
  object bigDecimal extends BigDecimalInstances

  object try_       extends TryInstances
  object tuple      extends TupleInstances
  object uuid       extends UUIDInstances
}
