/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats

package object syntax {
  object align extends AlignSyntax
  object all extends AllSyntaxBinCompat
  object alternative extends AlternativeSyntax
  object applicative extends ApplicativeSyntax
  object applicativeError extends ApplicativeErrorSyntax
  object apply extends ApplySyntax with ApplySyntaxBinCompat0
  object arrow extends ArrowSyntax
  object arrowChoice extends ArrowChoiceSyntax
  object bifunctor extends BifunctorSyntax
  object bifoldable extends BifoldableSyntax
  object binested extends BinestedSyntax
  object bitraverse extends BitraverseSyntax with BitraverseSyntaxBinCompat0
  @deprecated("use cats.syntax.semigroupal instead", "1.0.0-RC1")
  private[syntax] object cartesian extends SemigroupalSyntax
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
  object flatMap extends FlatMapSyntax with FlatMapOptionSyntax
  object foldable extends FoldableSyntax with FoldableSyntaxBinCompat0 with FoldableSyntaxBinCompat1
  object functor extends FunctorSyntax
  object functorFilter extends FunctorFilterSyntax
  object group extends GroupSyntax
  object hash extends HashSyntax
  object invariant extends InvariantSyntax
  object ior extends IorSyntax
  object list extends ListSyntax with ListSyntaxBinCompat0
  object monad extends MonadSyntax
  object monadError extends MonadErrorSyntax
  object monoid extends MonoidSyntax
  object nested extends NestedSyntax
  object option extends OptionSyntax
  object order extends OrderSyntax
  object parallel
      extends ParallelSyntax
      with ParallelTraverseSyntax
      with ParallelFlatSyntax
      with ParallelApplySyntax
      with ParallelBitraverseSyntax
      with ParallelUnorderedTraverseSyntax
      with ParallelFoldMapASyntax
      with ParallelTraverseFilterSyntax
      with ParallelReduceMapASyntax
  object partialOrder extends PartialOrderSyntax
  object profunctor extends ProfunctorSyntax
  object reducible extends ReducibleSyntax with ReducibleSyntaxBinCompat0
  object representable extends RepresentableSyntax
  object semigroup extends SemigroupSyntax
  object semigroupal extends SemigroupalSyntax
  object semigroupk extends SemigroupKSyntax
  object seq extends SeqSyntax
  object show extends ShowSyntax
  object strong extends StrongSyntax
  object try_ extends TrySyntax
  object traverse extends TraverseSyntax
  object traverseFilter extends TraverseFilterSyntax with TraverseFilterSyntaxBinCompat0
  object nonEmptyTraverse extends NonEmptyTraverseSyntax
  object unorderedFoldable extends UnorderedFoldableSyntax
  object unorderedTraverse extends UnorderedTraverseSyntax
  object validated extends ValidatedSyntax with ValidatedExtensionSyntax with ValidatedSyntaxBincompat0
  object vector extends VectorSyntax
  object writer extends WriterSyntax
  object set extends SetSyntax
  object nonEmptyAlternative extends NonEmptyAlternativeSyntax
}
