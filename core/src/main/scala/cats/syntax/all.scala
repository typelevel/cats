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

trait AllSyntax
    extends AlternativeSyntax
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

trait AllSyntaxBinCompat6 extends MonadErrorSyntaxBinCompat0
