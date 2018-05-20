package cats
package syntax

abstract class AllSyntaxBinCompat
    extends AllSyntax
    with AllSyntaxBinCompat0
    with AllSyntaxBinCompat1

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
    with NestedSyntax
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

trait AllSyntaxBinCompat0
    extends UnorderedTraverseSyntax
    with ApplicativeErrorExtension
    with TrySyntax

trait AllSyntaxBinCompat1 extends FlatMapOptionSyntax
