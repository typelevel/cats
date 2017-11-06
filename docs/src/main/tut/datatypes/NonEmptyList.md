---
layout: docs
title:  "NonEmptyList"
section: "data"
source: "core/src/main/scala/cats/data/NonEmptyList.scala"
scaladoc: "#cats.data.NonEmptyList"
---
# NonEmptyList

If you have had the opportunity of taking a look to [Validated](validated.html) or [IOR](ior.html), you'll find that a common case is to use `NonEmptyList` with one of these data structures.

Why? Because it fits nicely in the error reporting cases. As stated by its name, `NonEmptyList` is a _specialized_ data type that will have, at least, one element. You'll find that its behavior is like a list, with the aforementioned constraint. Think of it as a `List` wrapper.

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

### What is "Safety"?

`List` represents a singly-linked list data structure and it is formed by a `head` and a `tail`. The implementation in the Scala standard library has some methods for manipulating a `List` that perform _unsafe_ operations on it, that is, actions that can throw a runtime exception. Remember that one of the main advantages of Scala is its type system, enforced by the compiler.

So, for example, if we try to do this:

```tut:silent
val emptyList = List.empty[String]
```
```tut:book:fail
emptyList.head
emptyList.tail
```

You can see that we got an exception, and that's at runtime, not in compilation time because the laws defined for some operations of this structure denotes that you can't perform them _safely_ on an empty `List`. `headOption`, for example, is designed for safety, `head` don't.


## Defined for all its elements

An important trait of `NonEmptyList` is the totality. Consider the latter example in which we had an empty `List` and we performed a retrieval of its head and tail, both operations failing because there are no elements present in that `List`.
You can see this case as an example of partial functions. Specifically, either `head` and `tail` are partial because they are functions defined for a `List` that has at least one element.

`NonEmptyList` guarantees you that, all the operations that are executed to it to be defined because of the invariant about its size (at least one element) and therefore you can apply any transformation.