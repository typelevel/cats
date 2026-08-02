# Func

API Documentation: @:api(cats.data.Func)

`Func` is a function `A => F[B]`, where `F` is a functor. It is similar to `Kleisli`, but with weaker requirements: while `Kleisli` requires `FlatMap` (or `Monad`) for sequential composition, `Func` only requires `Functor` and supports parallel (applicative) composition via `AppFunc`.

## Kleisli vs Func

The key difference is in how they compose:

- **Kleisli** is for ''sequential'' composition: `A => F[B]` followed by `B => F[C]` requires the output of the first to feed into the second, and needs `FlatMap` to chain them.
- **Func** is for ''parallel'' composition: given `A => F[B]` and `A => F[C]`, we can combine them into `A => F[(B, C)]` using only `Applicative#product`.

This is useful when you have independent effects that don't depend on each other's results.

```scala mdoc:silent
import cats.data._
import cats.syntax.all._

val parseInt: Func[Option, String, Int] =
  Func.func(s => scala.util.Try(s.toInt).toOption)

val parseDouble: Func[Option, String, Double] =
  Func.func(s => scala.util.Try(s.toDouble).toOption)

// Compose in parallel: parse as Int and Double independently
val combined: Func[Option, String, (Int, Double)] =
  parseInt.product(parseDouble)

combined.run("42") // Some((42, 42.0))
```

## Func

At its core, `Func[F[_], A, B]` wraps a function `A => F[B]`. Depending on the properties of `F[_]`, we can do different things:

```scala mdoc:silent
val f: Func[Option, Int, Int] = Func.func(i => Some(i + 1))

// map only requires Functor
f.map(_ * 10).run(5) // Some(60)
```

### Methods

```
Method   | Constraint on `F[_]`
--------- | -------------------
map       | Functor
mapK      | (none - uses natural transformation)
product   | Applicative (via AppFunc)
```

## AppFunc

`AppFunc[F[_], A, B]` is the more powerful version of `Func`, requiring an `Applicative` instance for `F`. It supports parallel composition via `product`, `compose`, `andThen`, and `traverse`.

```scala mdoc:silent
val validateName: AppFunc[Either[String, *], Config, String] =
  Func.appFunc(c => if (c.name.nonEmpty) Right(c.name) else Left("Name required"))

val validateAge: AppFunc[Either[String, *], Config, Int] =
  Func.appFunc(c => if (c.age > 0) Right(c.age) else Left("Invalid age"))

// Both validators run independently
val combined = validateName.product(validateAge)
```

Note: the example above uses kind-projector syntax (`Either[String, *]`).

### Composition

`AppFunc` supports several composition patterns:

#### product

Combine two `AppFunc`s that share the same input type, producing a tuple of results:

```scala mdoc:silent
val f: AppFunc[Option, Int, Int] = Func.appFunc(i => Some(i + 1))
val g: AppFunc[Option, Int, String] = Func.appFunc(i => Some(i.toString))

f.product(g).run(42) // Some((43, "42"))
```

#### compose and andThen

Compose two `AppFunc`s where the output of one feeds into the other:

```scala mdoc:silent
val parse: AppFunc[Option, String, Int] =
  Func.appFunc(s => scala.util.Try(s.toInt).toOption)

val double: AppFunc[Option, Int, Int] =
  Func.appFunc(i => Some(i * 2))

// andThen: parse first, then double
val parseThenDouble: AppFunc[Nested[Option, Option, *], String, Int] =
  parse.andThen(double)

parseThenDouble.run("21") // Nested(Some(Some(42)))
```

#### traverse

Apply an `AppFunc` to each element of a traversable structure:

```scala mdoc:silent
val parse: AppFunc[Option, String, Int] =
  Func.appFunc(s => scala.util.Try(s.toInt).toOption)

parse.traverse(List("1", "2", "3")) // Some(List(1, 2, 3))
parse.traverse(List("1", "x", "3")) // None
```

### Methods

```
Method   | Constraint on `F[_]`
--------- | -------------------
map       | (none - Applicative already implies Functor)
product   | Applicative
compose   | Applicative
andThen   | Applicative
traverse  | Applicative
```

## Type class instances

`Func[F, C, *]` has the following type class instances depending on what `F[_]` has:

```
Type class     | Constraint on `F[_]`
-------------- | -------------------
Functor        | Functor
Apply          | Apply
Applicative    | Applicative
```

`Func[F, *, C]` (contravariant in the input type) has:

```
Type class     | Constraint on `F[_]`
-------------- | -------------------
Contravariant  | Contravariant
```

## When to use Func vs Kleisli

- Use **Kleisli** when you need sequential composition: the output of one function feeds into the next, and you need `flatMap`.
- Use **Func/AppFunc** when you have independent effects that can run in parallel: you only need `Applicative`, not `Monad`.

In practice, `Kleisli` is more commonly used because most real-world effects are sequential. However, `Func`/`AppFunc` can be more efficient for parallel validation, parallel data fetching, or any scenario where independent effects can be composed without dependencies.

## Further reading

- [The Essence of the Iterator Pattern](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf) - the paper that inspired `Func`
- @:api(cats.data.Kleisli) - the sequential counterpart
- @:api(cats.data.Nested) - used in `compose`/`andThen` return types
