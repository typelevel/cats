The typeclass pattern is a technique borrowed from haskell. You can
think of it as an "interface" in the java sense, but universally
quantified over some type. In scala they are encoded as a trait that
has one type variable, such as:

```scala
scala> /**
     |  * A typeclass to provide textual representation
     |  */
     | trait Show[A] {
     |   def show(f: A): String
     | }
defined trait Show
```
This class says that a value of type `Show[A]` has a way to turn `A`s
into `String`s. Now we can write a function which is polymorphic on
some A, as long as we have some value of Show[A], so that our function
can have a way of producing a String:

```scala
scala> def log[A](a: A)(implicit s: Show[A]) = println(s.show(a))
log: [A](a: A)(implicit s: Show[A])Unit
```

If we now try to call log, without supplying a Show instance, we will
get a compilation error:

```scala
scala> log("a string")
<console>:11: error: could not find implicit value for parameter s: Show[String]
              log("a string")
                 ^
```

It is trivial to supply a Show instance for String:

```scala
scala> implicit val stringShow = new Show[String] {
     |   def show(s: String) = s
     | }
stringShow: Show[String] = $anon$1@2ae77934

scala> // and now our call to Log succeeds
     | log("a string")
a string
```

For some types, providing a Show instance might depend on having some
implicit Show instance of some other type, for instance, we could
implement Show for Option:

```scala
scala> implicit def optionShow[A](implicit sa: Show[A]) = new Show[Option[A]] {
     |   def show(oa: Option[A]): String = oa match {
     |     case None => "None"
     |     case Some(a) => "Some("+ sa.show(a) + ")" 
     |   }
     | }
optionShow: [A](implicit sa: Show[A])Show[Option[A]]
```

Now we can call our log function with a `Option[String]` or a
`Option[Option[String]]`:

```scala
scala> log(Option(Option("hello")))
Some(Some(hello))
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
