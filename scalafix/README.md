# Scalafix rewrites for cats

## Try this!

Install the scalafix sbt plugin (globally or in a specific project):

```scala
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.5.0-M3")
```

run

```sh
sbt scalafix github:typelevel/cats/v1.0.0
```

## Available rewrites

- [x] All Unapply enabled methods, e.g. sequenceU, traverseU, etc. are removed. Unapply enabled syntax ops are also removed. Please use the partial unification SI-2712 fix instead. The easiest way might be this sbt-plugin.

- [x] The creation methods (left, right, apply, pure, etc.) in EitherT were improved to take less type arguments.

- [x] CartesianBuilder (i.e. |@|) syntax is deprecated, use the apply syntax on tuples instead. E.g. (x |@| y |@| z).map(...) should be replaced by (x, y, z).mapN(...)

- [x] Free.suspend is renamed to Free.defer for consistency.

- [x] traverse1_, intercalate1 and sequence1_ in Reducible were renamed to nonEmptyTraverse_, nonEmptyIntercalate and nonEmptySequence_ respectively.

- [x] cats.free.Inject is moved from cats-free to cats-core and renamed to cats.InjectK; cats.data.Prod is renamed to cats.data.Tuple2K; cats.data.Coproduct is renamed to cats.data.EitherK

- [x] Apply syntax on tuple (e.g. (x, y, z).map3(...)) was moved from cats.syntax.tuple._ to cats.syntax.apply._ and renamed to mapN, contramapN and imapN respectively.

- [x] Split is removed, and the method split is moved to Arrow. Note that only under CommutativeArrow does it guarantee the non-interference between the effects. see #1567
# WIP

- [ ] cats no longer publishes the all-inclusive bundle package "org.typelevel" % "cats", use cats-core, cats-free, or cats-law accordingly instead. If you need cats.free, use "org.typelevel" % "cats-free", if you need cats-laws use "org.typelevel" % "cats-laws", if neither, use "org.typelevel" % "cats-core".

- [ ] FunctorFilter, MonadCombine, MonadFilter, MonadReader, MonadState, MonadTrans, MonadWriter and TraverseFilter are no longer in cats, the functionalities they provided are inhereted by the new cats-mtl project. Please check here for migration guide.

- [ ] Several cats-core type class instances for cats.kernel were moved from their companion objects to separate traits and thus require imports from cats.instances.xxx._ (or the recommended import cats.implicits._) now. See #1659 for more details.

- [ ] foldLeftM is removed from Free, use foldM on Foldable instead, see #1117 for detail.

- [ ] iteratorFoldM was removed from Foldable due to #1716
