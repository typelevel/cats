---
layout: default
title:  "Imports"
section: "imports"
---
# Imports

The easiest approach to cats imports is to import everything that's commonly needed:

```tut:silent
import cats._
import cats.data._
import cats.implicits._
```

The `cats._` import brings in quite a few [type classes](http://typelevel.org/cats/typeclasses.html) (similar to interfaces) such as [Monad](http://typelevel.org/cats/tut/monad.html), [Semigroup](http://typelevel.org/cats/tut/semigroup.html), and [Foldable](http://typelevel.org/cats/tut/foldable.html). Instead of the entire `cats` package, you can import only the types that you need, for example:

```tut:silent
import cats.Monad
import cats.Semigroup
import cats.Foldable
```

The `cats.data._`, import brings in data structures such as [Validated](http://typelevel.org/cats/tut/validated.html) and [State](http://typelevel.org/cats/tut/state.html). Instead of the entire `cats.data` package, you can import only the types that you need, for example:

```tut:silent
import cats.data.Validated
import cats.data.State
```

The `cats.implicits._` import does a couple of things. Firstly, it brings in implicit type class instances for standard library types - so after this import you will have `Monad[List]` and `Semigroup[Int]` instances in implicit scope. Secondly, it adds syntax enrichment onto certain types to provide some handy methods such as right-biased `Either` combinators:

```tut:book
// cats adds right-biased combinators to the standard library's Either
val e: Either[String, Int] = Right(3)
e.map(_ + 1)

// cats adds an orEmpty method to the standard library's Option
val o: Option[String] = None
o.orEmpty
```

**Note**: if you import `cats.implicits._` (the preferred method), you should _not_ also use imports like `cats.syntax.option._` or `cats.instances.either._`. This can result in ambiguous implicit values that cause bewildering compile errors.
