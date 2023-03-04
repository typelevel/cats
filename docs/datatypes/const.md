# Const

API Documentation: @:api(cats.data.Const)

At first glance `Const` seems like a strange data type - it has two type parameters, yet only
stores a value of the first type. What possible use is it? As it turns out, it does
have its uses, which serve as a nice example of the consistency and elegance of functional programming.

## Thinking about `Const`
The `Const` data type can be thought of similarly to the `const` function, but as a data type.

```scala mdoc:silent
def const[A, B](a: A)(b: => B): A = a
```

The `const` function takes two arguments and simply returns the first argument, ignoring the second.

```scala mdoc:silent
case class Const[A, B](getConst: A)
```

The `Const` data type takes two type parameters, but only ever stores a value of the first type parameter.
Because the second type parameter is not used in the data type, the type parameter is referred to as a
"phantom type".

## Why do we care?
It would seem `Const` gives us no benefit over a data type that would simply not have the second type parameter.
However, while we don't directly use the second type parameter, its existence becomes useful in certain contexts.

### Example 1: Lens
The following is heavily inspired by [Julien Truffaut](https://github.com/julien-truffaut)'s
[blog post](http://functional-wizardry.blogspot.co.uk/2014/02/lens-implementation-part-1.html) on
[Monocle](https://github.com/julien-truffaut/Monocle), a fully-fledged optics library in Scala.

Types that contain other types are common across many programming paradigms. It is of course desirable in many
cases to get out members of other types, or to set them. In traditional object-oriented programming this is
handled by getter and setter methods on the outer object. In functional programming, a popular solution is
to use a lens.

A lens can be thought of as a first class getter/setter. A `Lens[S, A]` is a data type that knows how to get
an `A` out of an `S`, or set an `A` in an `S`.

```scala mdoc:silent
trait Lens[S, A] {
  def get(s: S): A

  def set(s: S, a: A): S

  def modify(s: S)(f: A => A): S =
    set(s, f(get(s)))
}
```

It can be useful to have effectful modifications as well - perhaps our modification can fail (`Option`) or
can return several values (`List`).

```scala mdoc:nest:silent
trait Lens[S, A] {
  def get(s: S): A

  def set(s: S, a: A): S

  def modify(s: S)(f: A => A): S =
    set(s, f(get(s)))

  def modifyOption(s: S)(f: A => Option[A]): Option[S] =
    f(get(s)).map(a => set(s, a))

  def modifyList(s: S)(f: A => List[A]): List[S] =
    f(get(s)).map(a => set(s, a))
}
```

Note that both `modifyOption` and `modifyList` share the *exact* same implementation. If we look closely, the
only thing we need is a `map` operation on the data type. Being good functional programmers, we abstract.

```scala mdoc:nest:silent
import cats.Functor
import cats.syntax.all._

trait Lens[S, A] {
  def get(s: S): A

  def set(s: S, a: A): S

  def modify(s: S)(f: A => A): S =
    set(s, f(get(s)))

  def modifyF[F[_] : Functor](s: S)(f: A => F[A]): F[S] =
    f(get(s)).map(a => set(s, a))
}
```

We can redefine `modify` in terms of `modifyF` by using `cats.Id`. We can also treat `set` as a modification
that simply ignores the current value. Due to these modifications however, we must leave `modifyF` abstract
since having it defined in terms of `set` would lead to infinite circular calls.

```scala mdoc:nest:silent
import cats.Id

trait Lens[S, A] {
  def modifyF[F[_] : Functor](s: S)(f: A => F[A]): F[S]

  def set(s: S, a: A): S = modify(s)(_ => a)

  def modify(s: S)(f: A => A): S = modifyF[Id](s)(f)

  def get(s: S): A
}
```

What about `get`? Certainly we can't define `get` in terms of the others.. the others are to modify an existing
value, whereas `get` is to retrieve it. Let's give it a shot anyways.

Looking at `modifyF`, we have an `S` we can pass in. The tricky part will be the `A => F[A]`, and then somehow
getting an `A` out of `F[S]`. If we imagine `F` to be a type-level constant function however, we could imagine
it would simply take any type and return some other constant type, an `A` perhaps. This suggests our `F` is a
`Const`.

We then take a look at the fact that `modifyF` takes an `F[_]`, a type constructor that takes a single type parameter.
`Const` takes two, so we must fix one. The function returns an `F[S]`, but we want an `A`, which implies we
have the first type parameter fixed to `A` and leave the second one free for the function to fill in as it wants.

Substituting in `Const[A, _]` wherever we see `F[_]`, the function wants an `A => Const[A, A]` and will give us back
a `Const[A, S]`. Looking at the definition of `Const`, we see that we only ever have a value of the first type parameter
and completely ignore the second. Therefore, we can treat any `Const[X, Y]` value as equivalent to `X` (plus or minus
some wrapping into `Const`). This leaves us with needing a function `A => A`. Given the type, the only thing we can do
is to take an `A` and return it right back (lifted into `Const`).

Before we plug and play however, note that `modifyF` has a `Functor` constraint on `F[_]`. This means we need to
define a `Functor` instance for `Const`, where the first type parameter is fixed.

*Note*: the example below assumes usage of the [kind-projector compiler plugin](https://github.com/typelevel/kind-projector) and will not compile if it is not being used in a project.

```scala mdoc:reset:silent
import cats.Functor
import cats.data.Const

implicit def constFunctor[X]: Functor[Const[X, *]] =
  new Functor[Const[X, *]] {
    // Recall Const[X, A] ~= X, so the function is not of any use to us
    def map[A, B](fa: Const[X, A])(f: A => B): Const[X, B] =
      Const(fa.getConst)
  }
```

Now that that's taken care of, let's substitute and see what happens.

```scala mdoc:silent
import cats.Id

trait Lens[S, A] {
  def modifyF[F[_] : Functor](s: S)(f: A => F[A]): F[S]

  def set(s: S, a: A): S = modify(s)(_ => a)

  def modify(s: S)(f: A => A): S = modifyF[Id](s)(f)

  def get(s: S): A = {
    val storedValue = modifyF[Const[A, *]](s)(a => Const(a))
    storedValue.getConst
  }
}
```

It works! We get a `Const[A, S]` out on the other side, and we simply just retrieve the `A` value stored inside.

What's going on here? We can treat the effectful "modification" we are doing as a store operation - we take an `A`
and store it inside a `Const`. Knowing only `F[_]` has a `Functor` instance, it can only `map` over the `Const`
which will do nothing to the stored value. After `modifyF` is done getting the new `S`, we retrieve the stored `A`
value and we're done!

### Example 2: Traverse
In the popular [The Essence of the Iterator Pattern](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf)
paper, Jeremy Gibbons and Bruno C. d. S. Oliveria describe a functional approach to iterating over a collection of
data. Among the abstractions presented are `Foldable` and `Traverse`, replicated below (also available in Cats).

```scala mdoc:silent
import cats.{Applicative, Monoid}

trait Foldable[F[_]] {
  // Given a collection of data F[A], and a function mapping each A to a B where B has a Monoid instance,
  // reduce the collection down to a single B value using the monoidal behavior of B
  def foldMap[A, B : Monoid](fa: F[A])(f: A => B): B
}

trait Traverse[F[_]] {
  // Given a collection of data F[A], for each value apply the function f which returns an effectful
  // value. The result of traverse is the composition of all these effectful values.
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}
```

These two type classes seem unrelated - one reduces a collection down to a single value, the other traverses
a collection with an effectful function, collecting results. It may be surprising to see that in fact `Traverse`
subsumes `Foldable`.

```scala mdoc:nest:silent
trait Traverse[F[_]] extends Foldable[F] {
  def traverse[G[_] : Applicative, A, X](fa: F[A])(f: A => G[X]): G[F[X]]

  def foldMap[A, B : Monoid](fa: F[A])(f: A => B): B
}
```

To start, we observe that if we are to implement `foldMap` in terms of `traverse`, we will want a `B` out
at some point. However, `traverse` returns a `G[F[X]]`. It would seem there is no way to unify these two.
However, if we imagine `G[_]` to be a sort of type-level constant function, where the fact that it's taking a
`F[X]` is irrelevant to the true underlying value, we can begin to see how we might be able to pull this off.

`traverse` however wants `G[_]` to have an `Applicative` instance, so let's define one for `Const`. Since
`F[X]` is the value we want to ignore, we treat it as the second type parameter and hence, leave it as the free
one.

```scala mdoc:nest:silent
import cats.data.Const

implicit def constApplicative[Z]: Applicative[Const[Z, *]] =
  new Applicative[Const[Z, *]] {
    def pure[A](a: A): Const[Z, A] = ???

    def ap[A, B](f: Const[Z, A => B])(fa: Const[Z, A]): Const[Z, B] = ???
  }
```

Recall that `Const[Z, A]` means we have a `Z` value in hand, and don't really care about the `A` type parameter.
Therefore we can more or less treat the type `Const[Z, A]` as just `Z`.

In functions `pure` and `ap` we have a problem. In `pure`, we have an `A` value, but want to return a `Z` value. We have
no function `A => Z`, so our only option is to completely ignore the `A` value. But we still don't have a `Z`! Let's
put that aside for now, but still keep it in the back of our minds.

In `ap` we have two `Z` values, and want to return a `Z` value. We could certainly return one or the other, but we
should try to do something more useful. This suggests composition of `Z`s, which we don't know how to do.

So now we need a constant `Z` value, and a binary function that takes two `Z`s and produces a `Z`. Sound familiar?
We want `Z` to have a `Monoid` instance!

```scala mdoc:nest:silent
implicit def constApplicative[Z : Monoid]: Applicative[Const[Z, *]] =
  new Applicative[Const[Z, *]] {
    def pure[A](a: A): Const[Z, A] = Const(Monoid[Z].empty)

    def ap[A, B](f: Const[Z, A => B])(fa: Const[Z, A]): Const[Z, B] =
      Const(Monoid[Z].combine(fa.getConst, f.getConst))
  }
```

We have our `Applicative`!

Going back to `Traverse`, we fill in the first parameter of `traverse` with `fa` since that's
the only value that fits.

Now we need a `A => G[B]`. We have an `A => B`, and we've decided to use `Const` for our `G[_]`. We need to
fix the first parameter of `Const` since `Const` takes two type parameters and `traverse` wants a type constructor
which only takes one. The first type parameter which will be the type of the actual values we store, and therefore will
be the type of the value we get out at the end, so we leave the second one free, similar to the `Applicative` instance.
We don't care about the second type parameter and there are no restrictions on it, so we can just use `Nothing`,
the type that has no values.

So to summarize, what we want is a function `A => Const[B, Nothing]`, and we have a function `A => B`. Recall
that `Const[B, Z]` (for any `Z`) is the moral equivalent of just `B`, so `A => Const[B, Nothing]` is equivalent
to `A => B`, which is exactly what we have, we just need to wrap it.

```scala mdoc:nest:silent
trait Traverse[F[_]] extends Foldable[F] {
  def traverse[G[_] : Applicative, A, X](fa: F[A])(f: A => G[X]): G[F[X]]

  def foldMap[A, B : Monoid](fa: F[A])(f: A => B): B = {
    val const: Const[B, F[Nothing]] = traverse[Const[B, *], A, Nothing](fa)(a => Const(f(a)))
    const.getConst
  }
}
```

Hurrah!

What's happening here? We can see `traverse` is a function that goes over a collection, applying an
effectful function to each value, and combining all of these effectful values. In our case, the effect
is mapping each value to a value of type `B`, where we know how to combine `B`s via its `Monoid` instance.
The `Monoid` instance is exactly what is used when `traverse` goes to collect the effectful values together.
Should the `F[A]` be "empty", it can use `Monoid#empty` as a value to return back.

Pretty nifty. `traverse`-ing over a collection with an effectful function is more general than traversing
over a collection to reduce it down to a single value.
