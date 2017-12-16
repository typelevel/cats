---
layout: docs
title:  "NonEmptyList"
section: "data"
source: "core/src/main/scala/cats/data/NonEmptyList.scala"
scaladoc: "#cats.data.NonEmptyList"
---
# NonEmptyList

The signature of the structure is as follows:

```scala
final case class NonEmptyList[+A](head: A, tail: List[A]) {
	// Implementation elided
}
```

The `head` of the `NonEmptyList` will be _non-empty_. Meanwhile, the `tail` can have zero or more elements.

For example:

```tut:book
import cats.data.NonEmptyList

val list = NonEmptyList.of("Hello")
list.head
```

You have seen the creation of a `NonEmptyList` with one `String` element ("Hello"), using `.of` method.
An `.of` advantage is that you can pass a set of elements for building a `NonEmptyList`, because it receives a `head` and a variadic `tail`, e.g.,

```tut:book
val list = NonEmptyList.of("Hello", "This", "is", "a", "NonEmptyList")
list.head
list.tail
```

Alternatively, you can create a `NonEmptyList` from a regular `List` like this:

```tut:book
val regularList = List("a", "b", "c", "d")
val nel = NonEmptyList.fromList(regularList)
```

But, what happens if you try to convert an empty `List` into a `NonEmptyList`? Let's see:

```tut:book
val emptyList = List.empty[String]
val nel = NonEmptyList.fromList(emptyList)
```

As you can see, `.fromList` is safe, giving you a `None` if you try to do the conversion. There's also an unsafe version of that operation: `.fromListUnsafe` that gives you the `NonEmptyList` if the conversion is successful or throws an `IllegalArgumentException` if you give it an empty `List`.

