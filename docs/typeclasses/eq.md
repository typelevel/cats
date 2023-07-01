# Eq

API Documentation: @:api(cats.kernel.Eq)

Eq is an alternative to the standard Java `equals` method.
It is defined by the single method `eqv`:

```scala
def eqv(x: A, y: A): Boolean
```

In Scala it's possible to compare any two values using `==` (which desugars to Java `equals`).
This is because `equals` type signature uses `Any` (Java's `Object`) to compare two values.
This means that we can compare two completely unrelated types without getting a compiler error.
The Scala compiler may warn us in some cases, but not all, which can lead to some weird bugs.
For example this code will raise a warning at compile time:


```scala mdoc:warn
42 == "Hello"
```

While this code will compile without a hitch:

```scala mdoc
"Hello" == 42
```

Ideally, Scala shouldn't let us compare two types that can never be equal.

As you can probably see in the type signature of `eqv`, it is impossible to compare two values of different types,
eliminating these types of bugs altogether.

The `Eq` syntax package also offers some handy symbolic operators:

```scala mdoc
import cats.syntax.all._

1 === 1

"Hello" =!= "World"
```

Implementing `Eq` instances yourself for every data type might seem like huge drawback compared to only slight gains of typesafety.
Fortunately for us, we have two great options. One option is to use inbuilt helper functions.
Another option is to use a small library called [kittens](https://github.com/typelevel/kittens), which can derive a lot of type class instances for our data types including `Eq`.

The first option using `Eq.fromUniversalEquals` only defers to `==` and works like this:

```scala mdoc
import cats.kernel.Eq
import cats.syntax.all._


case class Foo(a: Int, b: String)


implicit val eqFoo: Eq[Foo] = Eq.fromUniversalEquals


Foo(10, "") === Foo(10, "")
```


For an example using Kittens check out the [kittens repo](https://github.com/typelevel/kittens).
