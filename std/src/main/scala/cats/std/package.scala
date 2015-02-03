package cats

package object std {
  object all extends AllInstances

  object function extends FunctionInstances
  object list extends ListInstances
  object option extends OptionInstances
  object set extends SetInstances
  object stream extends StreamInstances
  object vector extends VectorInstances
}
