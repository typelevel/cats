# Eval

API Documentation: @:api(cats.Eval)

Eval is a data type for controlling synchronous evaluation.
Its implementation is designed to provide stack-safety at all times using a technique called trampolining.

There are two different factors that play into evaluation: memoization and laziness.

Memoized evaluation evaluates an expression only once and then remembers (memoizes) that value.
Lazy evaluation refers to when the expression is evaluated.
We talk about eager evaluation if the expression is immediately evaluated when defined and about lazy evaluation if the expression is evaluated when it's first used.

For example, in Scala, a `lazy val` is both lazy and memoized, a method definition `def` is lazy, but not memoized, since the body will be evaluated on every call.
A normal `val` evaluates eagerly and also memoizes the result.

`Eval` is able to express all of these evaluation strategies and allows us to chain computations using its `Monad` instance.

#### Eval.now

First of the strategies is eager evaluation, we can construct an `Eval` eagerly using `Eval.now`:


```scala mdoc
import cats.Eval
import cats.syntax.all._


val eager = Eval.now {
  println("Running expensive calculation...")
  1 + 2 * 3
}
```


We can run the computation using the given evaluation strategy anytime by using the `value` method.

```scala mdoc
eager.value

```

#### Eval.later

If we want lazy evaluation, we can use `Eval.later`:

```scala mdoc
val lazyEval = Eval.later {
  println("Running expensive calculation...")
  1 + 2 * 3
}

lazyEval.value

lazyEval.value
```

Notice that "Running expensive calculation" is printed only once, since the value was memoized internally.
`Eval.later` is different to using a `lazy val` in a few different ways.
First, it allows the runtime to perform garbage collection of the thunk after evaluation, leading to more memory being freed earlier.
Secondly, when `lazy val`s are evaluated, in order to preserve thread-safety, the Scala compiler will lock the whole surrounding class, whereas `Eval` will only lock itself.

#### Eval.always

If we want lazy evaluation, but without memoization akin to `Function0`, we can use `Eval.always`

```scala mdoc
val always = Eval.always {
  println("Running expensive calculation...")
  1 + 2 * 3
}

always.value

always.value
```

Here we can see, that the expression is evaluated every time we call `.value`.


### Chaining lazy computations

One of the most useful applications of `Eval` is its ability to chain together computations in a stack-safe way.
You can see one such usage when looking at the `foldRight` method found in [`Foldable`](../typeclasses/foldable.md).
Another great example are mutual tail-recursive calls:

```scala mdoc
object MutualRecursion {
  def even(n: Int): Eval[Boolean] =
    Eval.always(n == 0).flatMap {
      case true => Eval.True
      case false => odd(n - 1)
    }

  def odd(n: Int): Eval[Boolean] =
    Eval.always(n == 0).flatMap {
      case true => Eval.False
      case false => even(n - 1)
    }
}


MutualRecursion.odd(199999).value
```

Because `Eval` guarantees stack-safety, we can chain a lot of computations together using `flatMap` without fear of blowing up the stack.

You can also use `Eval.defer` to defer any computation that will return an `Eval[A]`.
This is useful, because nesting a call to `.value` inside any of the `Eval` creation methods can be unsafe.
