package cats

package object syntax {
  object all extends AllSyntax
  object invariant extends InvariantSyntax
  object contravariant extends ContravariantSyntax
  object functor extends FunctorSyntax
  object apply extends ApplySyntax
  object flatMap extends FlatMapSyntax
}
