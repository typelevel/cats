---
layout: docs
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

The `cats._` import brings in quite a few [type classes](http://typelevel.org/cats/typeclasses.html) (similar to interfaces) such as [Monad](http://typelevel.org/cats/typeclasses/monad.html), [Semigroup](http://typelevel.org/cats/typeclasses/semigroup.html), and [Foldable](http://typelevel.org/cats/typeclasses/foldable.html). Instead of the entire `cats` package, you can import only the types that you need, for example:

```tut:silent
import cats.Monad
import cats.Semigroup
import cats.Foldable
```

The `cats.data._`, import brings in data structures such as [Validated](http://typelevel.org/cats/datatypes/validated.html) and [State](http://typelevel.org/cats/datatypes/state.html). Instead of the entire `cats.data` package, you can import only the types that you need, for example:

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

If you'd like to import à la carte, you can do so, by importing from `cats.instances` for the type class instances and `cats.syntax` for syntax enrichment.
For example, if you'd like to import the `Semigroup` instance for `String` and the corresponding syntax:
```tut:book
import cats.instances.string._
import cats.syntax.semigroup._

"Hello, " |+| "World!"
```
The first import pulls the `Semigroup` instance for String into the scope, while the second import adds the `|+|` syntax.

You can also import all syntax or all instances by importing `cats.instances.all._` or `cats.syntax.all._` respectively.

**Note**: Beware that if you import a type class instance or its syntax twice, you will receive conflicting implicits with a less than helpful error message.
This usually happens when importing different type classes in the same hierarchy or when importing syntax enrichment for all type classes using `cats.syntax.all._` or `cats.implicits._` together with a more specific import like `cats.syntax.option._` or `cats.instances.either._`.
Below is an example of this phenomenon:
```tut:fail
import cats.instances.all._
import cats.syntax.semigroup._
val x = -2 |+| 1

//now we also need access to isEmpty from Monoid
import cats.syntax.monoid._
(x |+| 1).isEmpty
```

Compilation fails on the second invocation of `|+|` because we now have conflicting implicits from `Monoid` and `Semigroup`.
