package cats

package object std {
  object all      extends AllInstances

  object function extends FunctionInstances
  object list     extends ListInstances
  object option   extends OptionInstances
  object set      extends SetInstances
  object stream   extends StreamInstances
  object vector   extends VectorInstances
  
  object anyval   extends AnyValInstances
  object int      extends IntInstances
  object byte     extends ByteInstances
  object long     extends LongInstances
  object char     extends CharInstances
  object short    extends ShortInstances
  object float    extends FloatInstances
  object double   extends DoubleInstances
  object boolean  extends BooleanInstances
  object unit     extends UnitInstances
  object bigint   extends BigIntInstances
}
