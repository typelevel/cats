package cats

package object syntax {
  object all extends AllSyntaxBinCompat
  object alternative extends AlternativeSyntax
  object applicative extends ApplicativeSyntax
  object applicativeError extends ApplicativeErrorSyntax
  object apply extends ApplySyntax
  object arrow extends ArrowSyntax
  object arrowChoice extends ArrowChoiceSyntax
  object bifunctor extends BifunctorSyntax
  object bifoldable extends BifoldableSyntax
  object binested extends BinestedSyntax
  object bitraverse extends BitraverseSyntax
  @deprecated("use cats.syntax.semigroupal instead", "1.0.0-RC1")
  object cartesian extends SemigroupalSyntax
  object choice extends ChoiceSyntax
  object coflatMap extends CoflatMapSyntax
  object distributive extends DistributiveSyntax
  object eitherK extends EitherKSyntax
  object comonad extends ComonadSyntax
  object compose extends ComposeSyntax
  object contravariant extends ContravariantSyntax
  object contravariantSemigroupal extends ContravariantSemigroupalSyntax
  object contravariantMonoidal extends ContravariantMonoidalSyntax
  object either extends EitherSyntax with EitherSyntaxBinCompat0
  object eq extends EqSyntax
  object flatMap extends FlatMapSyntax
  object foldable extends FoldableSyntax
  object functor extends FunctorSyntax
  object functorFilter extends FunctorFilterSyntax
  object group extends GroupSyntax
  object invariant extends InvariantSyntax
  object ior extends IorSyntax
  object list extends ListSyntax with ListSyntaxBinCompat0
  object monad extends MonadSyntax
  object monadError extends MonadErrorSyntax
  object monoid extends MonoidSyntax
  object nested extends NestedSyntax
  object option extends OptionSyntax
  object order extends OrderSyntax
  object parallel extends ParallelSyntax with ParallelTraverseSyntax
  object partialOrder extends PartialOrderSyntax
  object profunctor extends ProfunctorSyntax
  object reducible extends ReducibleSyntax
  object representable extends RepresentableSyntax
  object semigroup extends SemigroupSyntax
  object semigroupal extends SemigroupalSyntax
  object semigroupk extends SemigroupKSyntax
  object show extends ShowSyntax
  object strong extends StrongSyntax
  object traverse extends TraverseSyntax
  object traverseFilter extends TraverseFilterSyntax
  object nonEmptyTraverse extends NonEmptyTraverseSyntax
  object unorderedFoldable extends UnorderedFoldableSyntax
  object unorderedTraverse extends UnorderedTraverseSyntax
  object validated extends ValidatedSyntax with ValidatedExtensionSyntax with ValidatedSyntaxBincompat0
  object vector extends VectorSyntax
  object writer extends WriterSyntax
  object set extends SetSyntax
}
