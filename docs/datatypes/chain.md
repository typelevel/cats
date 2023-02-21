# Chain

API Documentation: @:api(cats.data.Chain)

`Chain` is an immutable sequence data structure that allows constant time prepending, appending and concatenation.
This makes it especially efficient when used as a [Monoid], e.g. with [Validated] or [Writer].
As such it aims to be used where @:api(scala.collection.immutable.List) and @:api(scala.collection.immutable.Vector) incur a performance penalty.
Cats also includes type class implementations to support using `Chain` as a general-purpose collection type, including [Traverse], [Monad], and [Alternative].

## Motivation

`List` is a great data type, it is very simple and easy to understand.
It has very low overhead for the most important functions such as [fold][Foldable] and [map][Functor] and also supports prepending a single element in constant time.

Traversing a data structure with something like [Writer\[List\[Log\], A\]][Writer] or [ValidatedNel\[Error, A\]][Validated] is powerful and allows us to precisely specify what kind of iteration we want to do while remaining succinct.
However, in terms of efficiency it's a whole different story unfortunately.
That is because both of these traversals make use of the `List` monoid (or the [NonEmptyList] semigroup), which by the nature of `List` is very inefficient.
If you use [traverse][Traverse] with a data structure with `n` elements and [Writer] or [Validated] as the [Applicative] type, you will end up with a runtime of `O(n^2)`.
This is because, with `List`, appending a single element requires iterating over the entire data structure and therefore takes linear time.

So @:api(scala.collection.immutable.List) isn't all that great for this use case, so let's use @:api(scala.collection.immutable.Vector) or @:api(cats.data.NonEmptyVector)` instead, right?

Well, `Vector` has its own problems and in this case it's unfortunately not that much faster than `List` at all. You can check [this blog post](http://www.lihaoyi.com/post/BenchmarkingScalaCollections.html#vectors-are-ok) by Li Haoyi for some deeper insight into `Vector`'s issues.


`Chain` evolved from what used to be `fs2.Catenable` and Erik Osheim's [Chain](https://github.com/non/chain) library.
Similar to `List`, it is also a very simple data structure, but unlike `List` it supports constant O(1) time `append`, `prepend` and `concat`.
This makes its [Monoid] instance [super performant][Benchmarks] and a much better fit for usage with [Validated], [Writer], [Ior] or [Const].


## NonEmptyChain 

[NonEmptyChain][nec] is the non-empty version of `Chain`.
It does not have a [Monoid] instance since it cannot be empty, but it does have a [Semigroup] instance.
Likewise, it defines a [NonEmptyTraverse] instance, but no @:api(cats.TraverseFilter) instance.

To simplify the usage of `NonEmptyChain`, Cats includes type aliases like [ValidatedNec](validated.md#meeting-applicative) and [IorNec](ior.md#using-with-nonemptychain), as well as helper functions like `groupByNec` and `Validated.invalidNec`.

There are numerous ways to construct a `NonEmptyChain`, e.g. you can create one from a single element, a `NonEmptyList` or a `NonEmptyVector`:

```scala mdoc
import cats.data._

NonEmptyChain(1, 2, 3, 4)

NonEmptyChain.fromNonEmptyList(NonEmptyList(1, List(2, 3)))
NonEmptyChain.fromNonEmptyVector(NonEmptyVector(1, Vector(2, 3)))

NonEmptyChain.one(1)
```



You can also create an @:api(scala.Option) of `NonEmptyChain` from a `Chain` or any other collection type:

```scala mdoc
import cats.data._

NonEmptyChain.fromChain(Chain(1, 2, 3))
NonEmptyChain.fromSeq(List.empty[Int])
NonEmptyChain.fromSeq(Vector(1, 2, 3))
```

Sometimes, you'll want to prepend or append a single element to a chain and return the result as a `NonEmptyChain`:

```scala mdoc
import cats.data._

NonEmptyChain.fromChainAppend(Chain(1, 2, 3), 4)
NonEmptyChain.fromChainAppend(Chain.empty[Int], 1)
NonEmptyChain.fromChainPrepend(1, Chain(2, 3))
```
## How it works

`Chain` is implemented as a simple unbalanced binary tree ADT with four cases:
an empty `Chain` with no elements, a singleton `Chain` with exactly one element, a concatenation of two chains, or a wrapper for a @:api(scala.collection.immutable.Seq).

In code it looks like this:

```scala mdoc
sealed abstract class Chain[+A]

case object Empty extends Chain[Nothing]
case class Singleton[A](a: A) extends Chain[A]
case class Append[A](left: Chain[A], right: Chain[A]) extends Chain[A]
case class Wrap[A](seq: Seq[A]) extends Chain[A]
```

The `Append` constructor is what gives us the fast concatenation ability.
Concatenating two existing `Chain`s is just a call to the `Append` constructor, which is always constant time `O(1)`.

In case we want to append or prepend a single element,
 all we have to do is wrap the element with the `Singleton` constructor and then use the `Append` constructor to append or prepend the `Singleton` `Chain`.

The `Wrap` constructor lifts any `Seq` into a `Chain`.
This can be useful for concatenating already created collections that don't have great concatenation performance.
`Append(Wrap(list1), Wrap(list2)).foldMap(f)` will in general be much faster than just concatenating `list1` and `list2` and folding the result.

`Chain` doesn't directly expose the `Append` and `Wrap` constructors, because the arguments might refer to an empty `Chain` or `Seq`.
Instead of calling `Append` directly you can simply use `Chain.concat` or `++`, which will check if one of the arguments is empty:

```scala
def concat[A](c: Chain[A], c2: Chain[A]): Chain[A] =
  if (c.isEmpty) c2
  else if (c2.isEmpty) c
  else Append(c, c2)
```

To construct a `Chain` from a `Seq` you should use `Chain.fromSeq` which will also check for emptiness:

```scala
def fromSeq[A](s: Seq[A]): Chain[A] =
  if (s.isEmpty) nil
  else if (s.lengthCompare(1) == 0) one(s.head)
  else Wrap(s)
```

In conclusion `Chain` supports constant time concatenation, because it builds an unbalance tree of `Append`s.
`append` and `prepend` are treated as concatenation with single element collection to keep the same performance characteristics.
This unbalanced tree will always allow iteration in linear time. 

## Benchmarks

To get a good idea of performance of `Chain`, here are some benchmarks that test monoidal append (higher score is better):

```
Benchmark                                Mode  Cnt         Score         Error  Units
CollectionMonoidBench.accumulateChain   thrpt   25        81.973 ±       3.921  ops/s
CollectionMonoidBench.accumulateList    thrpt   25        21.150 ±       1.756  ops/s
CollectionMonoidBench.accumulateVector  thrpt   25        11.725 ±       0.306  ops/s
```

As you can see accumulating things with `Chain` is almost 4 times faster than `List` and nearly 8 times faster than `Vector`.
So appending is a lot more performant than the standard library collections, but what about operations like `map` or `fold`?
Fortunately we've also benchmarked these (again, higher score is better):

```
Benchmark                                Mode  Cnt          Score         Error  Units
ChainBench.consLargeChain               thrpt   25  143759156.264 ± 5611584.788  ops/s
ChainBench.consLargeList                thrpt   25  148512687.273 ± 5992793.489  ops/s
ChainBench.consLargeVector              thrpt   25    7249505.257 ±  202436.549  ops/s
ChainBench.consSmallChain               thrpt   25  119925876.637 ± 1663011.363  ops/s
ChainBench.consSmallList                thrpt   25  152664330.695 ± 1828399.646  ops/s
ChainBench.consSmallVector              thrpt   25   57686442.030 ±  533768.670  ops/s
ChainBench.createChainOption            thrpt   25  167191685.222 ± 1474976.197  ops/s
ChainBench.createChainSeqOption         thrpt   25   21264365.364 ±  372757.348  ops/s
ChainBench.createSmallChain             thrpt   25   87260308.052 ±  960407.889  ops/s
ChainBench.createSmallList              thrpt   25   20000981.857 ±  396001.340  ops/s
ChainBench.createSmallVector            thrpt   25   26311376.712 ±  288871.258  ops/s
ChainBench.createTinyChain              thrpt   25   75311482.869 ± 1066466.694  ops/s
ChainBench.createTinyList               thrpt   25   67502351.990 ± 1071560.419  ops/s
ChainBench.createTinyVector             thrpt   25   39676430.380 ±  405717.649  ops/s
ChainBench.foldLeftLargeChain           thrpt   25        117.866 ±       3.343  ops/s
ChainBench.foldLeftLargeList            thrpt   25        193.640 ±       2.298  ops/s
ChainBench.foldLeftLargeVector          thrpt   25        178.370 ±       0.830  ops/s
ChainBench.foldLeftSmallChain           thrpt   25   43732934.777 ±  362285.965  ops/s
ChainBench.foldLeftSmallList            thrpt   25   51155941.055 ±  882005.961  ops/s
ChainBench.foldLeftSmallVector          thrpt   25   41902918.940 ±   53030.742  ops/s
ChainBench.lengthLargeChain             thrpt   25     131831.918 ±    1613.341  ops/s
ChainBench.lengthLargeList              thrpt   25        271.015 ±       0.962  ops/s
ChainBench.mapLargeChain                thrpt   25         78.162 ±       2.620  ops/s
ChainBench.mapLargeList                 thrpt   25         73.676 ±       8.999  ops/s
ChainBench.mapLargeVector               thrpt   25        132.443 ±       2.360  ops/s
ChainBench.mapSmallChain                thrpt   25   24047623.583 ± 1834073.508  ops/s
ChainBench.mapSmallList                 thrpt   25   21482014.328 ±  387854.819  ops/s
ChainBench.mapSmallVector               thrpt   25   34707281.383 ±  382477.558  ops/s
ChainBench.reverseLargeChain            thrpt   25      37700.549 ±     154.942  ops/s
ChainBench.reverseLargeList             thrpt   25        142.832 ±       3.626  ops/s
```

While not dominant, `Chain` performance is in the middle of the pack for most operations benchmarked. 
`Chain` does have poor random access performance, and should be avoided in favor of `Vector` for random access heavy use cases.

Chain excels with concatenation heavy workloads and has comparable performance to `List` and `Vector` for most other operations. 
So next time you write any code that uses `List` or `Vector` as a [Monoid], be sure to use `Chain` instead!

> Note: All benchmarks above were run using JMH 1.32 with Scala 2.13.8 on JDK 11. 
For full details, see [here](https://github.com/typelevel/cats/pull/4264).
You can also check out the [benchmark source code](https://github.com/typelevel/cats/blob/v@VERSION@/bench/src/main/scala/cats/bench).


[nec]: @API_LINK_BASE@/cats/data/index.html#NonEmptyChain:cats.data.NonEmptyChainImpl.type