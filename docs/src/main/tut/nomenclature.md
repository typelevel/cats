---
layout: page
title:  "Glossary"
section: "Glossary"
position: 60
---

> - What is the difference between `unit` and `void`?
> - To discard the first value and keep only the first effect, is it `<*` or `*>`?
> - How do I make a computation `F[A]` fail by checking a condition on the value?

This page contains a catalogue of the major functions, type classes, and types aliases within the `cats` library. This catalogue serves, on one hand, as a bird's-eye view of the capabilities of each class. It is also intended as a go-to page for any `cats`-using developer, who may not remember the answer to any of the questions above.

Those looking for a printable version may want to check out the [cats-cheatsheet](https://arosien.github.io/cats-cheatsheets/typeclasses.pdf) file.

#### Simplifications

Because `cats` is a library for Scala, and because Scala has many more knobs and switches, the actual definitions and implementations of some functions in the library can seem a bit too obfuscated at first sight. To alleviate this, in this glossary we focus on the plain type signatures of the method, and ignore many of the details from Scala. In particular, in our type signatures:

- We use `A,B,C` for type variables of kind `*`, and `F, G, H` for type variables of a higher kind.
- We write type signatures in currified form: parameters are taken one at a time, and they are separated with the arrow `=>` operation. In Scala, a method's parameters may be split in several comma-separated lists.
- We do not differentiate between methods from the type-class trait (e.g. `trait Functor`), or the companion object, or the syntax companion (`implicit class`).
- For functions defined as method of the typeclass' trait, we ignore the receiver object.
- We ignore the implicit parameters. When a function requires another type-class constraint which is not the one in the section, we add the constraints on the side.
- In the parameters, we do not differentiate `Function` from `PartialFunction`. Instead, we use the arrow symbol `A => B` for both, and add a note if it is a `PartialFunction`.
- Some functions are defined through the [Partially Applied Type Params](http://typelevel.org/cats/guidelines.html#partially-applied-type-params) pattern. We ignore this.
- We ignore the distinction between by-name and by-value input parameters, so we just use the type for both. We use the notation `=> A`, without any parameters, to indicate constant functions.
- We ignore Scala variance annotations. We also ignore extra type parameters, which in some methods are added with a subtype-constraint, (e.g. `B >: A`). These are usually meant for flexibility, but we replace each one by its bound.

## Type-Classes of Kind `* -> *`

### Functor

| Type          | Method Name  |
| ------------- |--------------|
| `F[A] => F[Unit]`  | `void`   |
| `F[A] => B => F[B]`  | `as`   |
| `F[A] => (A => B) => F[B]` | `map`   |
| `F[A] => B => F[(B, A)]`  | `tupleLeft`  |
| `F[A] => B => F[(A, B)]`  | `tupleRight` |
| `(A => B) => (F[A] => F[B])` | `lift`   |

### Apply

| Type          | Method Name | Symbol   |
| ------------- |--------------|------------|
| `F[A] => F[A] => F[A]` | `productL`  | `<*`
| `F[A] => F[B] => F[B]` | `productR`  | `*>`
| `F[A] => F[B] => F[(A,B)]` | `product`  |
| `F[A => B] => F[A] => F[B]` | `ap`  |  `<*>`
| `F[A] => F[B] => (A => B => C) => F[C]` | `map2` |

### Applicative

| Type          | Method Name | Notes   |
| ------------- |--------------|------------|
| `A => F[A]`   | `pure` |
| `=> F[Unit]`  | `unit` |
| `Boolean => F[Unit] => F[Unit]` | `when`   | Performs effect iff condition is true
|                                 | `unless` | Adds effect iff condition is false

### FlatMap / Monad

| Type          | Method Name |
| ------------- |---------------|
| `F[F[A]] => F[A]` | `flatten`  |
| `F[A] => (A => F[B]) => F[B]` | `flatMap`
| `F[A] => (A => F[B]) => F[(A,B)]` | `productM`
| `F[Boolean] => F[A] => F[A] => F[A]` | `ifM`
| `F[A] => (A => F[B]) => F[A]` | `flatTap`


### FunctorFilter

| Type        | Method Name   | Notes  |
|-------------|---------------|--------|
| `F[A] => (A => Boolean) => F[A]` |   `filter` |
| `F[A] => (A => Option[B]) => F[B]` | `mapFilter` |
| `F[A] => (A => B) => F[B]` | `collect` | The `A => B` is a PartialFunction
| `F[Option[A]] => F[A]` | `flattenOption` |


### `ApplicativeError[E, F]`

The source code of `cats` uses the `E` type variable for the error type.

| Type         | Method Name  | Notes |
|--------------|--------------|-------|
| `E => F[A]`   | `raiseError`   |
| `F[A] => F[Either[E,A]]`    | `attempt`     |
| `F[A] => (E => A) => F[A]`  | `handleError`   |
| `F[A] => (E => F[A]) => F[A]` | `handleErrorWith`  |
| `F[A] => (E => A) => F[A]` | `recover`  |  The `E => A` is a PartialFunction.
| `F[A] => (E => F[A]) => F[A]` | `recoverWith`  |  The `E => F[A]` is a PartialFunction.
| `F[A] => (E => F[Unit]) => F[A]` | `onError`  | The `E => F[Unit]` is a PartialFunction.
| `Either[E,A] => F[A]` | `fromEither` |
| `Option[A] => E => F[A]` | `liftFromOption` |

### `MonadError[E, F]`

| Type          | Method Name  | Notes  |
| ------------- |--------------|--------|
| `F[A] => E => (A => Boolean) => F[A]` | `ensure`
| `F[A] => (A => E) => (A => Boolean) => F[A]` | `ensureOr`
| `F[A] => (E => E) => F[A]`  | `adaptError` | The `E => E` is a PartialFunction.
| `F[Either[E,A]] => F[A]`     | `rethrow`


### `UnorderedFoldable`

| Type          | Method Name  | Constraints
| ------------- |--------------|----------------
| `F[A] => Boolean` | `isEmpty` |
| `F[A] => Boolean` | `nonEmpty` |
| `F[A] => Long` | `size` |
| `F[A] => (A => Boolean) => Boolean`| `forall` |
| `F[A] => (A => Boolean) => Boolean`| `exists` |
| `F[A] => A`  | `unorderedFold` | `A: CommutativeMonoid`
| `F[A] => (A => B) => B`| `unorderedFoldMap` | `B: CommutativeMonoid`


### `Foldable`

| Type          | Method Name  | Constraints
| ------------- |--------------|-----------
| `F[A] => B => ((B,A) => B) => F[B]` | `foldLeft`
| `F[A] => (A => G[B]) => G[B]` | `foldMapM` | `G: Monad` and `B: Monoid`

### Reducible

| Type          | Method Name  | Constraints
| ------------- |--------------|-----------
| `F[A] => ((A,A) => A) => A` | `reduceLeft`  |
| `F[A] => A` | `reduce`  | `A: Semigroup`

### Traverse

| Type         | Method Name  | Constraints |
|------------|--------------|-----------|
| `F[G[A]] => G[F[A]]`  | `sequence` | `G: Applicative` |
| `F[A] => (A => G[B]) => G[F[B]]` | `traverse` | `G: Applicative` |
| `F[A] => (A => G[F[B]]) => G[F[B]]` | `flatTraverse` | `F: FlatMap` and `G: Applicative`
| `F[G[F[A]]] => G[F[A]]` | `flatSequence` | `G: Applicative` and `F: FlatMap`
| `F[A] => F[(A,Int)]` | `zipWithIndex` |
| `F[A] => ((A,Int) => B) => F[B]` | `mapWithIndex` |


## Transformers

### Constructors and wrappers

Most monad transformers and data types come down to a

| Data Type  | is an alias or wrapper of |
|------------|--------------|
| `OptionT[F[_], A]`    | `F[Option[A]]`
| `EitherT[F[_], A, B]` | `F[Either[A,B]`
| `Kleisli[F[_], A, B]` | `A => F[B]`
| `Reader[A, B]` | `A => B`
| `ReaderT[F[_], A, B]` | `Kleisli[F, A, B]`
| `Writer[A, B]` | `(A,B)`
| `WriterT[F[_], A, B]` | `F[(A,B)]`
| `Tuple2K[F[_], G[_], A]` | `(F[A], G[A])`
| `EitherK[F[_], G[_], A]` | `Either[F[A], G[A]]`
| `FunctionK[F[_], G[_]`   | `F[X] => G[X]` for every `X`
| `F ~> G`   | Alias of `FunctionK[F, G]`

### `OptionT`

For convenience, in these types we use the symbol `OT` to abbreviate `OptionT`.

| Type     | Method Name  | Constraints |
|----------|--------------|-------------|
| `=> OT[F, A]` | `none` | `F: Applicative` |
| `A => OT[F, A]` | `some` or `pure` | `F: Applicative`
| `F[A] => OT[F, A]` | `liftF`  | `F: Functor`
| `OT[F, A] => F[Option[A]]` | `value`
| `OT[F, A] => (A => B) => OT[F, B]` | `map`  | `F: Functor`
| `OT[F, A] => (F ~> G) => OT[G, B]` | `mapK`
| `OT[F, A] => (A => Option[B]) => OT[F, B]` | `mapFilter` | `F: Functor`
| `OT[F, A] => B => (A => B) => F[B]` | `fold` or `cata`
| `OT[F, A] => (A => OT[F, B]) => OT[F,B]` | `flatMap`
| `OT[F, A] => (A => F[Option[B]]) => F[B]` | `flatMapF`  | `F: Monad` |
| `OT[F, A] => A => F[A]` | `getOrElse` | `F: Functor`  |
| `OT[F, A] => F[A] => F[A]` | `getOrElseF` | `F: Monad`  |
| `OT[F, A] => OT[F, A] => OT[F, A]` |

### `EitherT`

For convenience, in these types we use the symbol `ET` to abbreviate `EitherT`. In these signatures, we use the type variables `A` and `B` to indicate the left and right sides of the `Either`.

| Type     | Method Name  | Constraints |
|----------|--------------|-------------|
| `A => ET[F, A, B]` | `leftT` | `F: Applicative` |
| `B => ET[F, A, B]` | `rightT` | `F: Applicative` |
|                    | `pure` | `F: Applicative` |
| `F[A] => ET[F, A, B]` | `left`  | `F: Applicative` |
| `F[B] => ET[F, A, B]` | `right` | `F: Applicative` |
|                       | `liftF` | `F: Applicative` |
| `Either[A, B] => ET[F, A, B]` | `fromEither` | `F: Applicative` |
| `Option[B] => A => ET[F, A, B]` | `fromOption` | `F: Applicative` |
| `F[Option[B]] => A => ET[F, A, B]` | `fromOptionF` | `F: Functor` |
| `Boolean => B => A => ET[F, A, B]` | `cond`   | `F: Applicative` |
| `ET[F, A, B] => (A => C) => (B => C) => F[C]` | `fold` | `F: Functor` |
| `ET[F, A, B] => ET[F, B, A]` | `swap` | `F: Functor` |
| `ET[F, A, A] => F[A]`        | `merge` |

### `Kleisli`, a.k.a `ReaderT`

For convenience, in this section we use the symbol `Ki` to abbreviate `Kleisli`

| Type     | Method Name  | Constraints |
|----------|--------------|-------------|
| `Ki[F, A, B] => (A => F[B])` | `run`  |
| `Ki[F, A, B] => A => F[B]` | `apply`  |
| `A => Ki[F, A, A]` | `ask` | `F: Applicative`
| `B => Ki[F, A, B]` | `pure` | `F: Applicative`
| `F[B] => Ki[F, A, B]` | `liftF` |
| `Ki[F, A, B] => (C => A) => Ki[F, C, B]` | `local` |
| `Ki[F, A, B] => Ki[F, A, A]` | `tap` |
| `Ki[F, A, B] => (B => C) => Ki[F, A, C]` | `map` |
| `Ki[F, A, B] => (F ~> G) => Ki[G, A, B]` | `mapK` |
| `Ki[F, A, B] => (F[B] => G[C]) => Ki[F, A, C]` | `mapF` |
| `Ki[F, A, B] => Ki[F, A, F[B]]` | `lower` |


## Type Classes of Kind `(*,*) => *`

For these type-classes, we sometimes use the infix type notation `A F B` instead of `F[A, B]`. The goal of this notation is to reinforce the idea that many of these type-classes represent "functions with effects".

### Bifunctor

| Type          | Method Name  |
| ------------- |--------------|
| `F[A,B] => (A => C) => F[C,B]` | `leftMap` |
| `F[A,B] => (B => D) => F[A,D]` | `.rightFunctor` and `.map` |
| `F[A,B] => (A => C) => (B => D) => F[C,D]` | `bimap` |

#### Profunctor

| Type           | Method Name  |
--------|-------------
| `F[A,B] => (B => C) => F[A,C]` | `rmap`  |
| `F[A,B] => (C => A) => F[C,B]` | `lmap`  |
| `F[A,B] => (C => A) => (B => D) => F[C,D]` | `dimap`  |

#### Strong Profunctor

| Type  | Method Name |
--------|-------------|
| `F[A,B] => ( (A, C) F (B, C) )` | `first`  |
| `F[A,B] => ( (C, A) F (C, B) )` | `second` |

#### Compose -- Category -- Choice

| Type  | Method Name | Symbol |
--------|-------------|--------------|
| `F[A,B] => F[C,A] => F[C,B]` | `compose` | `<<<` |
| `F[A,B] => F[B,C] => F[A,C]` | `andThen` | `>>>` |
| `=> F[A,A]` | `id`  |
| `F[A,B] => F[C,B] => F[(A|C), B]` | `choice` | `|||`
| `=> (A|A) F A` | `codiagonal` |

#### Arrow

| Type           | Method Name  | Symbol |
|----------------|--------------|--------------|
| `(A => B) => F[A,B]`  | `lift`    |
| `F[A,B] => F[C,D] => F[(A,C),(B,D)]` | `split` | `***` |
| `F[A,B] => F[A,C] => F[A, (B,C)]` | `merge` | `&&&` |

#### ArrowChoice

| Type  | Method Name | Symbol |
--------|-------------|--------------|
| `F[A,B] => F[C,D] => F[Either[A, C], Either[B, D]]` | `choose` | `+++`
| `F[A,B] => F[Either[A, C], Either[B, C]]` | `left`  |
| `F[A,B] => F[Either[C, A], Either[C, B]]` | `right` |

