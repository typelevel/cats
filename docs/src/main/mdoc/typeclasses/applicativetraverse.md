---
layout: docs
title:  "Applicative and Traversable Functors"
section: "typeclasses"
scaladoc: "#cats.Functor"
---

# Applicative and Traversable Functors

## An example from the standard library

One of the most useful functions when working with `scala.concurrent.Future` is `Future.traverse`, presented below
in a simplified form.

```scala mdoc:silent
import scala.concurrent.{ExecutionContext, Future}

def traverseFuture[A, B](as: List[A])(f: A => Future[B])(implicit ec: ExecutionContext): Future[List[B]] =
  Future.traverse(as)(f)
```

`traverseFuture` takes a `List[A]` and for each `A` in the list applies the function `f` to it, gathering results
as it goes along. `f` is often referred to as an *effectful* function, where the `Future` effect is running the computation
concurrently, presumably on another thread. This effect is apparent in the result of the function, which has
gathered results inside `Future`.

But what if the effect we wanted wasn't `Future`? What if instead of concurrency for our effect we wanted validation
(`Option`, `Either`, `Validated`) or `State` ? It turns out we can abstract out the commonalities between all these
data types and write a generic `traverse` function once and for all. We can even go further and abstract over data
types that can be traversed over such as `List`, `Vector`, and `Option`.

In this series we will build up the machinery needed to generalize the standard library's `Future.traverse` into
its fully abstract and most reusable form.

If you'd like to read the published literature on these ideas, some good starting points are
"[Applicative Programming with Effects][applicativeProgramming]" by McBride and Patterson, and
"[The Essence of the Iterator Pattern][iterator]" by Gibbons and Oliveira.

[applicativeProgramming]: http://www.staff.city.ac.uk/~ross/papers/Applicative.html "Applicative Programming with Effects"
[iterator]: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf "The Essence of the Iterator Pattern"
