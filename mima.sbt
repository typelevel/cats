import com.typesafe.tools.mima.core.ProblemFilters._
import com.typesafe.tools.mima.core._

ThisBuild / mimaBinaryIssueFilters ++= {
  // These things are Ops classes that shouldn't have the `value` exposed. These should have never been public because they don't
  // provide any value. Making them private because of issues like #2514 and #2613.
  Seq(
    exclude[DirectMissingMethodProblem]("cats.ApplicativeError#LiftFromOptionPartially.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.Const#OfPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.EitherT#CondPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.EitherT#FromEitherPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.EitherT#FromOptionPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.EitherT#LeftPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.EitherT#LeftTPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.EitherT#PurePartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.EitherT#RightPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.IorT#BothTPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.IorT#CondPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.IorT#FromEitherPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.IorT#FromIorPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.IorT#FromOptionPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.IorT#LeftPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.IorT#LeftTPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.IorT#PurePartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.IorT#RightPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.NonEmptyChainOps.value"),
    exclude[DirectMissingMethodProblem]("cats.data.OptionT#FromOptionPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.OptionT#PurePartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.data.Validated#CatchOnlyPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.free.Free#FreeInjectKPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.free.Free#FreeLiftInjectKPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.free.FreeT#FreeTLiftInjectKPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ApplicativeErrorIdOps.e"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ApplicativeErrorOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ApplicativeIdOps.a"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ApplicativeOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ApplyOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.BinestedIdOps.value"),
    exclude[DirectMissingMethodProblem]("cats.syntax.BitraverseOps.fab"),
    exclude[DirectMissingMethodProblem]("cats.syntax.DistributiveOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.EitherIdOps.obj"),
    exclude[DirectMissingMethodProblem]("cats.syntax.EitherIdOpsBinCompat0.value"),
    exclude[DirectMissingMethodProblem]("cats.syntax.EitherSyntax#CatchOnlyPartiallyApplied.dummy"),
    exclude[DirectMissingMethodProblem]("cats.syntax.EitherKOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.EitherObjectOps.either"),
    exclude[DirectMissingMethodProblem]("cats.syntax.EitherOps.eab"),
    exclude[DirectMissingMethodProblem]("cats.syntax.EitherOpsBinCompat0.value"),
    exclude[DirectMissingMethodProblem]("cats.syntax.FlatMapIdOps.a"),
    exclude[DirectMissingMethodProblem]("cats.syntax.FlatMapOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.FlatMapOptionOps.fopta"),
    exclude[DirectMissingMethodProblem]("cats.syntax.FlattenOps.ffa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.FoldableOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.GuardOps.condition"),
    exclude[DirectMissingMethodProblem]("cats.syntax.IfMOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.IndexOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.IorIdOps.a"),
    exclude[DirectMissingMethodProblem]("cats.syntax.LeftOps.left"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ListOps.la"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ListOpsBinCompat0.la"),
    exclude[DirectMissingMethodProblem]("cats.syntax.MonadErrorOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.MonadErrorRethrowOps.fea"),
    exclude[DirectMissingMethodProblem]("cats.syntax.MonadIdOps.a"),
    exclude[DirectMissingMethodProblem]("cats.syntax.MonadOps.fa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.MonoidOps.lhs"),
    exclude[DirectMissingMethodProblem]("cats.syntax.NestedBitraverseOps.fgagb"),
    exclude[DirectMissingMethodProblem]("cats.syntax.NestedFoldableOps.fga"),
    exclude[DirectMissingMethodProblem]("cats.syntax.NestedIdOps.value"),
    exclude[DirectMissingMethodProblem]("cats.syntax.NestedReducibleOps.fga"),
    exclude[DirectMissingMethodProblem]("cats.syntax.OptionIdOps.a"),
    exclude[DirectMissingMethodProblem]("cats.syntax.OptionOps.oa"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ParallelApOps.ma"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ParallelFlatSequenceOps.tmta"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ParallelFlatTraversableOps.ta"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ParallelSequence_Ops.tma"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ParallelSequenceOps.tma"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ParallelTraversable_Ops.ta"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ParallelTraversableOps.ta"),
    exclude[DirectMissingMethodProblem]("cats.syntax.RightOps.right"),
    exclude[DirectMissingMethodProblem]("cats.syntax.SeparateOps.fgab"),
    exclude[DirectMissingMethodProblem]("cats.syntax.SetOps.se"),
    exclude[DirectMissingMethodProblem]("cats.syntax.TabulateOps.f"),
    exclude[DirectMissingMethodProblem]("cats.syntax.TryOps.self"),
    exclude[DirectMissingMethodProblem]("cats.syntax.UniteOps.fga"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ValidatedExtension.self"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ValidatedIdOpsBinCompat0.a"),
    exclude[DirectMissingMethodProblem]("cats.syntax.ValidatedIdSyntax.a"),
    exclude[DirectMissingMethodProblem]("cats.syntax.VectorOps.va"),
    exclude[DirectMissingMethodProblem]("cats.syntax.WriterIdSyntax.a")
  ) ++ // Only compile-time abstractions (macros) allowed here
    Seq(
      exclude[IncompatibleMethTypeProblem]("cats.arrow.FunctionKMacros.lift"),
      exclude[MissingTypesProblem]("cats.arrow.FunctionKMacros$"),
      exclude[IncompatibleMethTypeProblem]("cats.arrow.FunctionKMacros#Lifter.this"),
      exclude[IncompatibleResultTypeProblem]("cats.arrow.FunctionKMacros#Lifter.c"),
      exclude[DirectMissingMethodProblem]("cats.arrow.FunctionKMacros.compatNewTypeName")
    ) ++ // package private classes no longer needed
    Seq(
      exclude[MissingClassProblem]("cats.kernel.compat.scalaVersionMoreSpecific$"),
      exclude[MissingClassProblem]("cats.kernel.compat.scalaVersionMoreSpecific"),
      exclude[MissingClassProblem](
        "cats.kernel.compat.scalaVersionMoreSpecific$suppressUnusedImportWarningForScalaVersionMoreSpecific"
      )
    ) ++ // New issues found since mima 0.8.0 (#3596, #3641)
    Seq(
      exclude[NewMixinForwarderProblem]("cats.kernel.Band#mcI#sp.combineN"),
      exclude[NewMixinForwarderProblem]("cats.kernel.Band#mcD#sp.combineN"),
      exclude[NewMixinForwarderProblem]("cats.kernel.Band#mcJ#sp.combineN"),
      exclude[NewMixinForwarderProblem]("cats.kernel.Band.combineN"),
      exclude[NewMixinForwarderProblem]("cats.kernel.Band#mcF#sp.combineN")
    ) ++ // Additional methods in package-private traits
    Seq(
      exclude[ReversedMissingMethodProblem]("cats.data.NonEmptyCollection.grouped")
    ) ++ // https://github.com/typelevel/cats/pull/3785
    Seq(
      exclude[MissingClassProblem]("cats.syntax.EqOps$mcJ$sp"),
      exclude[MissingClassProblem]("cats.syntax.EqOps$mcD$sp"),
      exclude[FinalClassProblem]("cats.syntax.EqOps"),
      exclude[MissingFieldProblem]("cats.syntax.EqOps.lhs"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.unapply"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.apply"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.lhs"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.copy"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.copy$default$1"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.productPrefix"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.productArity"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.productElement"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.productIterator"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.canEqual"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.copy$default$1$mcD$sp"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.copy$default$1$mcF$sp"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.copy$default$1$mcJ$sp"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.copy$default$1$mcI$sp"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.productElementNames"),
      exclude[DirectMissingMethodProblem]("cats.syntax.EqOps.productElementName"),
      exclude[MissingClassProblem]("cats.syntax.EqOps$"),
      exclude[MissingClassProblem]("cats.syntax.EqOps$mcF$sp"),
      exclude[MissingClassProblem]("cats.syntax.EqOps$mcI$sp")
    ) ++ // https://github.com/typelevel/cats/pull/3918
    Seq(
      exclude[MissingClassProblem]("algebra.laws.IsSerializable"),
      exclude[MissingClassProblem]("algebra.laws.IsSerializable$")
    ) ++ // https://github.com/typelevel/cats/pull/3987
    Seq(
      exclude[DirectAbstractMethodProblem]("cats.free.ContravariantCoyoneda.k"),
      exclude[ReversedAbstractMethodProblem]("cats.free.ContravariantCoyoneda.k"),
      exclude[DirectAbstractMethodProblem]("cats.free.Coyoneda.k"),
      exclude[ReversedAbstractMethodProblem]("cats.free.Coyoneda.k")
    ) ++ // https://github.com/typelevel/cats/pull/4315
    Seq(
      exclude[MissingClassProblem]("cats.compat.compat$package"),
      exclude[MissingClassProblem]("cats.compat.compat$package$")
    )
}

ThisBuild / mimaBinaryIssueFilters ++= {
  if (tlIsScala3.value)
    Seq(
      exclude[DirectMissingMethodProblem]("cats.free.ContravariantCoyoneda.unsafeApply"),
      exclude[DirectMissingMethodProblem]("cats.free.Coyoneda.unsafeApply")
    )
  else Nil
}
