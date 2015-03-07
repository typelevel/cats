The typeclass pattern is a technique borrowed from haskell. You can
think of it as an "interface" in the java sense, but universally
quantified over some type. In scala they are encoded as a trait that
has one type variable, such as:

```tut
/**
 * A typeclass to provide textual representation
 */
trait Show[A] {
  def show(f: A): String
}
```
This class says that a value of type `Show[A]` has a way to turn `A`s
into `String`s. Now we can write a function which is polymorphic on
some A, as long as we have some value of Show[A], so that our function
can have a way of producing a String:

```tut
def log[A](a: A)(implicit s: Show[A]) = println(s.show(a))
```

If we now try to call log, without supplying a Show instance, we will
get a compilation error:

```tut:nofail
log("a string")
```

It is trivial to supply a Show instance for String:

```tut
implicit val stringShow = new Show[String] {
  def show(s: String) = s
}
// and now our call to Log succeeds
log("a string")
```

For some types, providing a Show instance might depend on having some
implicit Show instance of some other type, for instance, we could
implement Show for Option:

```tut
implicit def optionShow[A](implicit sa: Show[A]) = new Show[Option[A]] {
  def show(oa: Option[A]): String = oa match {
    case None => "None"
    case Some(a) => "Some("+ sa.show(a) + ")" 
  }
}
```

Now we can call our log function with a `Option[String]` or a
`Option[Option[String]]`:

```tut
log(Option(Option("hello")))
```

Scala has syntax just for this pattern that we use frequently:

```scala
def log[A : Show](a: A) = println(implicitly[Show[A]].show(a))
```

is the same as

```scala
def log[A](a: A)(implicit s: Show[A]) = println(s.show(a))
```

That is that declaring the type parameter as `A : Show`, it will add
an implicit parameter to the method signature (with a name we do not know).
