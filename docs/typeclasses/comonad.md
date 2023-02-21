# Comonad

API Documentation: @:api(cats.Comonad)

`Comonad` is a `Functor` and provides duals of the [`Monad`](monad.md) `pure`
and `flatMap` functions.  A dual to a function has the same types but the 
direction of the arrows are reversed. Whether or not that is useful, or even possible, 
depends on the particular type. For a more formal definition of duality, please 
refer to [https://ncatlab.org/nlab/show/duality](https://ncatlab.org/nlab/show/duality).

### extract

Monads have `pure` from `Applicative` which gives you the ability to wrap 
a value `A` using the type constructor giving an `F[A]`. Comonad has 
`extract` which instead takes an `F[A]` and extracts the `A`. Therefore, to be 
able to implement extract we must have a type of which we are certain
we can get an `A` from an `F[A]`. For example we cannot always get an `A` 
from a `List[A]` because if the list is empty there is nothing to get.

For the same reason, `Option` doesn't have a `Comonad` instance, because we 
cannot always get an `A` from an Option, it may be empty too.

Some examples that we can implement `Comonad` for include `OneAnd`, `Tuple2` 
and the "non empty" collections.

First some imports.

```scala mdoc:silent
import cats._
import cats.data._
import cats.syntax.all._
import cats.instances.list._
```

`NonEmptyList` has a `Comonad` instance and its implementation of `extract` 
simply returns the head element of the list, which we know we will always
have.

```scala mdoc
NonEmptyList.of(1,2,3).extract
```

### coflatMap

`coflatMap` is the dual of Monad's `flatMap`.  While `flatMap` allows us to chain 
together operations in a monadic context, `coflatMap` takes a value in some context
 `F[A]` and a function `F[A] => B` and returns a new value in a context `F[B]`.

The default implementation of `coflatMap` for `NonEmptyList` will pass the supplied
function with the whole list, then the tail of that, then the tail of that and so 
on. This is illustrated below.

```scala mdoc
NonEmptyList.of(1,2,3,4,5).coflatMap(identity)
```

# CoflatMap

While `FlatMap` is a weaker version of `Monad` that doesn't have the `pure` function, 
`CoflatMap` is a `Comonad` without the `extract` function. There are many instances
of type classes in Cats that implement `CoflatMap` but not `Comonad`.

For example we cannot write `extract` for `Option[A]` because there's no way to 
pull an `A` out of nowhere if the Option is empty.

```scala mdoc:silent
def extract[A](fa : Option[A]): A = fa match {
	case Some(a) => a
	case None => ??? // What now?
}
```

Another example is `List`. Remember we cannot write `extract` for list because lists
can be empty, but we can implement the `coflatMap` and it works identically to the
one shown above for `NonEmptyList`.

```scala mdoc
List(1,2,3,4,5).coflatMap(identity)
```
