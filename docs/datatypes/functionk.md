# FunctionK

API Documentation: @:api(cats.arrow.FunctionK)

A `FunctionK` transforms values from one first-order-kinded type (a type that takes a single type
parameter, such as `List` or `Option`) into another first-order-kinded type. This transformation is
universal, meaning that a `FunctionK[List, Option]` will translate all `List[A]` values into an
`Option[A]` value for all possible types of `A`. This explanation may be easier to understand if we
first step back and talk about ordinary functions.

## Ordinary Functions
Consider the following scala method:

```scala mdoc:silent
def first(l: List[Int]): Option[Int] = l.headOption
```

This isn't a particularly helpful method, but it will work as an example. Instead of writing this as a
method, we could have written this as a function _value_:

```scala mdoc:silent
val first: List[Int] => Option[Int] = l => l.headOption
```

And here, `=>` is really just some syntactic sugar for `Function1`, so we could also write that as:

```scala mdoc:silent:reset
val first: Function1[List[Int], Option[Int]] = l => l.headOption
````

Let's cut through the syntactic sugar even a little bit further. `Function1` isn't really a special type.
It's just a trait that looks something like this:

```scala mdoc:silent
// we are calling this `MyFunction1` so we don't collide with the actual `Function1`
trait MyFunction1[A, B] {
  def apply(a: A): B
}
```

So if we didn't mind being a bit verbose, we could have written our function as:

```scala mdoc:silent:reset
val first: Function1[List[Int], Option[Int]] = new Function1[List[Int], Option[Int]] {
  def apply(l: List[Int]): Option[Int] = l.headOption
}
```

## Abstracting via Generics

Recall our `first` method:

```scala mdoc:silent:reset
def first(l: List[Int]): Option[Int] = l.headOption
```

The astute reader may have noticed that there's really no reason that this method needs to be tied directly to `Int`. We could use generics to make this a bit more general:

```scala mdoc:silent:reset
def first[A](l: List[A]): Option[A] = l.headOption
```

But how would we represent this new `first` method as a `=>`/`Function1` value? We are looking for something like a type of `List[A] => Option[A] forAll A`, but this isn't valid scala syntax. `Function1` isn't quite the right fit, because its `apply` method doesn't take a generic type parameter.

## Higher Kinds to the Rescue

It turns out that we can represent our universal `List` to `Option` transformation with something that looks a bit like `Function1` but
that adds a type parameter to the `apply` method and utilizes higher kinds:

```scala mdoc:silent:reset
trait MyFunctionK[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
```

Cats provides this type as `FunctionK` (we used `MyFunctionK` for our example type to avoid confusion). So now we can write `first` as a `FunctionK[List, Option]` value:

```scala mdoc:silent
import cats.arrow.FunctionK

val first: FunctionK[List, Option] = new FunctionK[List, Option] {
  def apply[A](l: List[A]): Option[A] = l.headOption
}
```

## Syntactic Sugar

If the example above looks a bit too verbose for you, the [kind-projector](https://github.com/typelevel/kind-projector)
compiler plugin [provides](https://github.com/typelevel/kind-projector#polymorphic-lambda-values) a more concise syntax.
After adding the plugin to your project, you could write the `first` example as:

```scala mdoc:silent:nest
val first: FunctionK[List, Option] = λ[FunctionK[List, Option]](_.headOption)
```

Cats also provides a `~>` type alias for `FunctionK`, so an even more concise version would be:

```scala mdoc:silent:nest
import cats.~>

val first: List ~> Option = λ[List ~> Option](_.headOption)
```

Being able to use `~>` as an alias for `FunctionK` parallels being able to use `=>` as an alias for `Function1`.

## Use-cases

`FunctionK` tends to show up when there is abstraction over higher-kinds. For example, interpreters for [free monads](freemonad.md) and [free applicatives](freeapplicative.md) are represented as `FunctionK` instances.

## Types with more than one type parameter

Earlier it was mentioned that `FunctionK` operates on first-order-kinded types (types that take a single type parameter such as `List` or `Option`). It's still possible to use `FunctionK` with types that would normally take more than one type parameter (such as `Either`) if we fix all of the type parameters except for one. For example:

```scala mdoc:silent
type ErrorOr[A] = Either[String, A]

val errorOrFirst: FunctionK[List, ErrorOr] =
  λ[FunctionK[List, ErrorOr]](_.headOption.toRight("ERROR: the list was empty!"))
```

## Natural Transformation

In category theory, a [natural transformation](https://ncatlab.org/nlab/show/natural+transformation) provides a morphism between Functors while preserving the internal structure. It's one of the most fundamental notions of category theory.

If we have two Functors `F` and `G`, `FunctionK[F, G]` is a natural transformation via parametricity. That is, given `fk: FunctionK[F, G]`, for all functions `A => B` and all `fa: F[A]` the following are equivalent:
```Scala
fk(F.map(fa)(f)) <-> G.map(fk(fa))(f)
```

We don't need to write a law to test the implementation of the `fk` for the above to be true. It's automatically given by parametricity.

Thus natural transformation can be implemented in terms of `FunctionK`. This is why a parametric polymorphic function `FunctionK[F, G]` is sometimes referred as a natural transformation. However, they are two different concepts that are not isomorphic. 

For more details, Bartosz Milewski has written a great blog post titled
["Parametricity: Money for Nothing and Theorems for Free"](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/).
 
