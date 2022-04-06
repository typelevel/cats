# Imports

The easiest approach to Сats imports is to import everything that's commonly needed:

```scala mdoc:reset:silent
import cats._
import cats.data._
import cats.syntax.all._
```

The `cats._` import brings in quite a few [type classes](typeclasses.html) (similar to interfaces) such as [Monad](typeclasses/monad.html), [Semigroup](typeclasses/semigroup.html), and [Foldable](typeclasses/foldable.md). Instead of the entire `cats` package, you can import only the types that you need, for example:

```scala mdoc:reset:silent
import cats.Monad
import cats.Semigroup
import cats.Foldable
```

The `cats.data._`, import brings in data structures such as [Validated](datatypes/validated.html) and [State](datatypes/state.md). Instead of the entire `cats.data` package, you can import only the types that you need, for example:

```scala mdoc:reset:silent
import cats.data.Validated
import cats.data.State
```

The `cats.syntax.all._` import adds syntax enrichment onto certain types to provide some handy methods such as right-biased `Either` combinators:

```scala mdoc:reset
import cats.syntax.all._

// Сats adds right-biased combinators to the standard library's Either
val e: Either[String, Int] = Right(3)
e.map(_ + 1)

// cats adds an orEmpty method to the standard library's Option
val o: Option[String] = None
o.orEmpty
```

If you'd like to import à la carte, you can do so, by importing from `cats.syntax` for syntax enrichment.
For example, if you'd like to import the `Semigroup` syntax:

```scala mdoc:reset
import cats.syntax.semigroup._

"Hello, " |+| "World!"
```

The import adds the `|+|` syntax.

**Note**: Beware that if you import a type class instance or its syntax twice, you will receive conflicting implicits with a less than helpful error message.
This usually happens when importing different type classes in the same hierarchy or when importing syntax enrichment for all type classes using `cats.syntax.all._` or `cats.implicits._` together with a more specific import like `cats.syntax.option._` or `cats.instances.either._`.
Below is an example of this phenomenon:

```scala mdoc:reset:silent:fail
import cats.syntax.all._

1 |+| 2

import cats.syntax.semigroup._

3 |+| 5 // error: value |+| is not a member of Int
```

Compilation fails on the second invocation of `|+|` because we now have conflicting implicits from `Semigroup`.
