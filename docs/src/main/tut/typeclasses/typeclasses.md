# Type classes

The type class pattern is a ubiquitous pattern in Scala, its function
is to provide a behavior for some type. You think of it as an
"interface" in the Java sense. Here's an example.

```tut:silent
/**
 * A type class to provide textual representation
 */
trait Show[A] {
  def show(f: A): String
}
```
This class says that a value of type `Show[A]` has a way to turn `A`s
into `String`s. Now we can write a function which is polymorphic on
some `A`, as long as we have some value of `Show[A]`, so that our function
can have a way of producing a `String`:

```tut:silent
def log[A](a: A)(implicit s: Show[A]) = println(s.show(a))
```

If we now try to call log, without supplying a `Show` instance, we will
get a compilation error:

```tut:nofail
log("a string")
```

It is trivial to supply a `Show` instance for `String`:

```tut:silent
implicit val stringShow = new Show[String] {
  def show(s: String) = s
}
```

and now our call to Log succeeds

```tut:book
log("a string")
```

This example demonstrates a powerful property of the type class
pattern. We have been able to provide an implementation of `Show` for
`String`, without needing to change the definition of `java.lang.String`
to extend a new Java-style interface; something we couldn't have done
even if we wanted to, since we don't control the implementation of
`java.lang.String`. We use this pattern to retrofit existing
types with new behaviors. This is usually referred to as "ad-hoc
polymorphism".

For some types, providing a `Show` instance might depend on having some
implicit `Show` instance of some other type, for instance, we could
implement `Show` for `Option`:

```tut:silent
implicit def optionShow[A](implicit sa: Show[A]) = new Show[Option[A]] {
  def show(oa: Option[A]): String = oa match {
    case None => "None"
    case Some(a) => "Some("+ sa.show(a) + ")"
  }
}
```

Now we can call our log function with a `Option[String]` or a
`Option[Option[String]]`:

```tut:book
log(Option(Option("hello")))
```

Scala has syntax just for this pattern that we use frequently:

```tut:silent
def log[A: Show](a: A) = println(implicitly[Show[A]].show(a))
```

is the same as

```tut:silent
def log[A](a: A)(implicit s: Show[A]) = println(s.show(a))
```

That is that declaring the type parameter as `A : Show`, it will add
an implicit parameter to the method signature (with a name we do not know).
