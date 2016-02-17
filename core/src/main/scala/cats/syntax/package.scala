package cats

package object syntax {
  object all extends AllSyntax
  object apply extends ApplySyntax
  object bifunctor extends BifunctorSyntax
  object bifoldable extends BifoldableSyntax
  object cartesian extends CartesianSyntax
  object coflatMap extends CoflatMapSyntax
  object coproduct extends CoproductSyntax
  object comonad extends ComonadSyntax
  object compose extends ComposeSyntax
  object contravariant extends ContravariantSyntax
  object either extends EitherSyntax
  object eq extends EqSyntax
  object flatMap extends FlatMapSyntax
  object foldable extends FoldableSyntax
  object functor extends FunctorSyntax
  object group extends GroupSyntax
  object invariant extends InvariantSyntax
  object monadCombine extends MonadCombineSyntax
  object monadFilter extends MonadFilterSyntax
  object option extends OptionSyntax
  object order extends OrderSyntax
  object partialOrder extends PartialOrderSyntax
  object profunctor extends ProfunctorSyntax
  object semigroup extends SemigroupSyntax
  object semigroupk extends SemigroupKSyntax
  object show extends Show.ToShowOps
  object split extends SplitSyntax
  object streaming extends StreamingSyntax
  object strong extends StrongSyntax
  object transLift extends TransLiftSyntax
  object traverse extends TraverseSyntax
  object xor extends XorSyntax
  object validated extends ValidatedSyntax
}
