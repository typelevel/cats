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
package syntax

abstract class AllSyntaxBinCompat
    extends AllSyntax
    with AllSyntaxBinCompat0
    with AllSyntaxBinCompat1
    with AllSyntaxBinCompat2
    with AllSyntaxBinCompat3
    with AllSyntaxBinCompat4
    with AllSyntaxBinCompat5
    with AllSyntaxBinCompat6
    with AllSyntaxBinCompat7

trait AllSyntax
    extends AlternativeSyntax
    with NonEmptyAlternativeSyntax
    with AlignSyntax
    with ApplicativeSyntax
    with ApplicativeErrorSyntax
    with ApplySyntax
    with ArrowSyntax
    with ArrowChoiceSyntax
    with BifunctorSyntax
    with BifoldableSyntax
    with BitraverseSyntax
    with SemigroupalSyntax
    with CoflatMapSyntax
    with ComonadSyntax
    with ComposeSyntax
    with ContravariantSyntax
    with DecidableSyntax
    with DistributiveSyntax
    with ContravariantMonoidalSyntax
    with ContravariantSemigroupalSyntax
    with EitherKSyntax
    with EitherSyntax
    with EqSyntax
    with FlatMapSyntax
    with FoldableSyntax
    with FunctorSyntax
    with GroupSyntax
    with HashSyntax
    with InvariantSyntax
    with IorSyntax
    with ListSyntax
    with MonadErrorSyntax
    with MonadSyntax
    with MonoidSyntax
    with OptionSyntax
    with OrderSyntax
    with ParallelSyntax
    with PartialOrderSyntax
    with ProfunctorSyntax
    with ReducibleSyntax
    with SemigroupSyntax
    with SemigroupKSyntax
    with ShowSyntax
    with StrongSyntax
    with TraverseSyntax
    with NonEmptyTraverseSyntax
    with ValidatedSyntax
    with VectorSyntax
    with WriterSyntax
    with ParallelFoldMapASyntax
    with ParallelTraverseFilterSyntax
    with ParallelReduceMapASyntax

trait AllSyntaxBinCompat0 extends UnorderedTraverseSyntax with ApplicativeErrorExtension with TrySyntax

trait AllSyntaxBinCompat1
    extends FlatMapOptionSyntax
    with ChoiceSyntax
    with NestedSyntax
    with BinestedSyntax
    with ParallelFlatSyntax
    with SetSyntax
    with ValidatedExtensionSyntax
    with RepresentableSyntax

trait AllSyntaxBinCompat2
    extends ParallelTraverseSyntax
    with TraverseFilterSyntax
    with FunctorFilterSyntax
    with EitherSyntaxBinCompat0
    with ListSyntaxBinCompat0
    with ValidatedSyntaxBincompat0

trait AllSyntaxBinCompat3 extends UnorderedFoldableSyntax with Function1Syntax

trait AllSyntaxBinCompat4
    extends TraverseFilterSyntaxBinCompat0
    with ApplySyntaxBinCompat0
    with ParallelApplySyntax
    with FoldableSyntaxBinCompat0
    with ReducibleSyntaxBinCompat0
    with FoldableSyntaxBinCompat1
    with BitraverseSyntaxBinCompat0

trait AllSyntaxBinCompat5 extends ParallelBitraverseSyntax

trait AllSyntaxBinCompat6 extends ParallelUnorderedTraverseSyntax

trait AllSyntaxBinCompat7 extends SeqSyntax
