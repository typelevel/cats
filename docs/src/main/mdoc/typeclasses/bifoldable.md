---
layout: docs
title:  "Bifoldable"
section: "typeclasses"
source: "core/src/main/scala/cats/Bifoldable.scala"
scaladoc: "#cats.Bifoldable"
---

# Bifoldable

`Bifoldable` takes two type parameters and it's seen as a `Foldable` in both of these parameters.

As a reminder `Foldable` is implemented in terms of `foldLeft` and `foldRight`, similarly `Bifoldable` is implemented in terms of:

   ```scala
   //eagerly performs a left-associative bi-fold over `fab` 
    def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C

    //lazily performs a right-associative bi-fold over `fab` 
    def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]
   ```

and by implementing those 2 methods you also get:
   ```scala
     def bifold[A, B](fab: F[A, B])(implicit A: Monoid[A], B: Monoid[B]): (A, B)
  
     def bifoldMap[A, B, C](fab: F[A, B])(f: A => C, g: B => C)(implicit C: Monoid[C]): C
   ```

## Either and Validated as Bifoldable

Assume in your codebase multiple input paths and system requests end up as `Either` or `Validated`.  
We want to track the amount of errors and store the content of valid requests.

Let's add the implicits:
```scala mdoc
import cats._
import cats.data._
import cats.implicits._
```

and define a summary class, capable of storing this info:
```scala mdoc
case class Report(content: List[String], errors: Int) {
  def add(`new`: String): Report =
    this.copy(content = content :+ `new`)

  def reject(): Report =
    this.copy(errors = errors + 1)
}
```

`Bifoldable` is useful to get a summary value from `F[_,_]` data types:
```scala mdoc
def grow[T[_, _]: Bifoldable](current: Report)(one: T[_, String]): Report =
  one.bifoldLeft(current)((acc, _) => acc.reject(), (acc, user) => acc.add(user))
```

Here is the system input:
```scala mdoc
val validated =
  List(
    Validated.valid[Exception, String]("valid request 1"),
    Validated.valid[Exception, String]("valid request 2"),
    Validated.invalid[Exception, String](new RuntimeException("Not a valid request"))
  )

val attempted =
  List(
    Either.right[Exception, String]("valid request 1"),
    Either.right[Exception, String]("valid request 2"),
    Either.left[Exception, String](new RuntimeException("Not a valid request"))
  )
```

and bi-fold each value into the accumulator:
```scala mdoc
val empty = Report(List.empty, 0)

validated
  .foldl(empty)((acc, validation) => grow(acc)(validation))

attempted
  .foldl(empty)((acc, attempt) => grow(acc)(attempt))
```

## Tuple as Bifoldable

Assume we have `(String, String, Int)`  and to get our summary we need `_1` and `_3`.
The existing implementations `(*,*)`, `(T0, *, *)`, `(T0, T1, *, *)` ..  aren't useful,

Let's make our own `Bifoldable`:
```scala mdoc
implicit def bifoldableForTuple3[A0]: Bifoldable[(*, A0, *)] =
  new Bifoldable[(*, A0, *)] {
    def bifoldLeft[A, B, C](fa: (A, A0, B), c: C)(f: (C, A) => C, g: (C, B) => C): C =
      g(f(c, fa._1), fa._3)

    def bifoldRight[A, B, C](fa: (A, A0, B), c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
      g(fa._3, f(fa._1, c))
  }
```
Note: Bifoldable doesn't inherit other typeclasses so the implementation is straight forward.

```scala mdoc
//(name, age, occupation)
val description = ("Niki", 22, "Developer")
  
Bifoldable[(*, Int, *)]
  .bifoldLeft(description, List.empty[String])((acc, name) => acc :+ name, (acc, occupation) => acc :+ occupation)

Bifoldable[(*, Int, *)]
    .bifold(description)

Bifoldable[(*, Int, *)]
    .bifoldMap(description)(name => List(name), occupation => List(occupation))
```

## Bifunctor `compose`

If `F[_,_]` and `G[_,_]` have `Bifunctor` instances then so does `F[G[_,_],G[_,_]]`.

Examples for `(Either[*, *], Either[*, *])` and `((*, *), (*, *))`:
```scala mdoc
Bifoldable[(*, *)]
  .compose(Bifoldable[(*, *)])
  .bifoldLeft((("name1 ", 1000),("name2 ", 2000)), List.empty[String])((acc, name) => acc :+ name, (acc, _) => acc)

Bifoldable[(*, *)]
  .compose(Bifoldable[Either[*, *]])
  .bifold((Left(1), Right(2)))
```

