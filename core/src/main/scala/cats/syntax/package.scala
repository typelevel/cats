package cats

package object syntax {
  object all extends AllSyntax
  object apply extends ApplySyntax
  object bifunctor extends BifunctorSyntax
  object coflatMap extends CoflatMapSyntax
  object comonad extends ComonadSyntax
  object compose extends ComposeSyntax
  object contravariant extends ContravariantSyntax
  object eq extends EqSyntax
  object flatMap extends FlatMapSyntax
  object foldable extends FoldableSyntax
  object functor extends FunctorSyntax
  object invariant extends InvariantSyntax
  object monadCombine extends MonadCombineSyntax
  object monadFilter extends MonadFilterSyntax
  object order extends OrderSyntax
  object partialOrder extends PartialOrderSyntax
  object profunctor extends ProfunctorSyntax
  object semigroup extends SemigroupSyntax
  object semigroupk extends SemigroupKSyntax
  object show extends Show.ToShowOps
  object split extends SplitSyntax
  object strong extends StrongSyntax
  object traverse extends TraverseSyntax
}
