# Show

API Documentation: @:api(cats.Show)

Show is an alternative to the Java `toString` method.
It is defined by a single function `show`:

```scala
def show(a: A): String
```

You might be wondering why you would want to use this, considering `toString` already serves the same purpose and case classes already provide sensible implementations for `toString`.
The difference is that `toString` is defined on `Any`(Java's `Object`) and can therefore be called on anything, not just case classes.
Most often, this is unwanted behaviour, as the standard implementation of `toString` on non case classes is mostly gibberish.
Consider the following example:

```scala mdoc
(new {}).toString
```

The fact that this code compiles is a design flaw of the Java API.
We want to make things like this impossible, by offering the `toString` equivalent as a type class, instead of the root of the class hierarchy.
In short, `Show` allows us to only have String-conversions defined for the data types we actually want.

To make things easier, Cats defines a few helper functions to make creating `Show` instances easier.

```scala
/** creates an instance of Show using the provided function */
def show[A](f: A => String): Show[A]

/** creates an instance of Show using object toString */
def fromToString[A]: Show[A]
```

These can be used like this:

```scala mdoc
import cats.Show

case class Person(name: String, age: Int)

implicit val showPerson: Show[Person] = Show.show(person => person.name)

case class Department(id: Int, name: String)

implicit val showDep: Show[Department] = Show.fromToString
```


This still may not seem useful to you, because case classes already automatically implement `toString`, while `show` would have to be implemented manually for each case class.
Thankfully with the help of a small library called [kittens](https://github.com/typelevel/kittens) a lot of type class instances including `Show` can be derived automatically!

Cats also offers `Show` syntax to make working with it easier.
This includes the `show` method which can be called on anything with a `Show` instance in scope:

```scala mdoc
import cats.syntax.all._

val john = Person("John", 31)

john.show
```

It also includes a String interpolator, which works just like the standard `s"..."` interpolator, but uses `Show` instead of `toString`:

```scala mdoc
val engineering = Department(2, "Engineering")
show"$john works at $engineering"
```
