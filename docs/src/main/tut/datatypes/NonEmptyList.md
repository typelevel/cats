---
layout: docs
title:  "NonEmptyList"
section: "data"
source: "core/src/main/scala/cats/data/NonEmptyList.scala"
scaladoc: "#cats.data.NonEmptyList"
---
# NonEmptyList

If you have had the opportunity of taking a look to [Validated](validated.html) or [IOR](ior.html), you'll find that one of the most common case is to use `NonEmptyList` with one of these data structures.

Why? Because it fits nicely in the error reporting cases. As its stated by its name, `NonEmptyList` is a _specialized_ data type that will have, at least, one element. You'll find its behaviour like a list, with the aforementioned constraint. Think of it as a `List` wrapper.

The signature of the structure is as follows:

```scala
final case class NonEmptyList[+A](head: A, tail: List[A]) {
	// Implementation elided
}
```

The `head` of the will be _non empty_, meanwhile, the `tail` can have zero or more elements.

For example:

```tut:book
import cats.data.NonEmptyList

val list = NonEmptyList.of("Hello")
list.head
```

You've seen the creation of a `NonEmptyList` with one `String` element (_"Hello"_), using `.of` combinator, but also you can create a `NonEmptyList` from a regular `List` like this:

```tut:book
val regularList = List("a", "b", "c", "d")
val nel = NonEmptyList.fromList(regularList)
```

But, what happen if you try to convert an empty `List` into a `NonEmptyList`? Let's see:

```tut:book
val emptyList = List.empty[String]
val nel = NonEmptyList.fromList(emptyList)
```

As you can see, `.fromList` is type safe, giving you a `None` if you try to do the conversion. There's also an unsafe version of that combinator: `.fromListUnsafe` that gives you the `NonEmptyList` if the conversion is successful or an `IllegalArgumentException` if you give it an empty `List`.

## All is about safety

`List` has a lot of unsafe methods because it doesn't enforce any constraint (other than its type). If you do this:

```tut:silent
val emptyList = List.empty[String]
```
```tut:book:fail
emptyList.head
```

You can see that you get a `NoSuchElementException` because you're trying to get the head of an empty `List`. The same behaviour applies with methods like `.last` or `.tail`.

Using `NonEmptyList` you'll have the type safety in compile-time for this data type.