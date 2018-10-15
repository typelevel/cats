---
layout: page
title:  "Nomenclature"
section: "Nomenclature"
position: 60
---

This page provides a table-like list of each function in `cats`, together with its name. It is intended for people who may need to quickly look up what is the name of a function from its type.

Those looking for a printable version may want to check out the [cats-cheatsheet](https://arosien.github.io/cats-cheatsheets/typeclasses.pdf) file. 


### Conventions used in this nomenclature 

- We use `A,B,C` for type variables of kind-0 (data types), we use `F, G, H` for type variables of kind `* -> *`, and we use `P,Q,R` for type variables of a more complex kind. Some exceptions are `E`, for errors. 
- In each type signature, we use the type variables for each kind in the order in which they appear left-to-right. 
- Computations that are only used for effects, and whose value is discarded, are represented as `F[-]`, 

Also, for simplicity, we have made some ommissions to the type signatures: 

- Type signatures are currified, parameters are taken one at a time, and they are separated with the arrow `=>` operation. This simplifies Scala's use of several lists of parameters. 
- We ignore from the type signatures any implicit parameters, and instead add them as constraints at the side. 
- We ignore the distinction between by-name and by-value input parameers, so we just use the type for both. Instead, we use the notation `=> A`, without any parameters, to indicate constant functions. 
- We ignore the distinction between the Scala traits of `Function` and `PartialFunction[A, B]`. Thus, we use in both cases the symbol `A => B` to indicate both a `Function[A,B]`, and a `PartialFunction[A,B]`; and for partial functions we add a note besides it. 
- We ignore variance annotations and extra parameters added for variance flexibility, such as `B >: A`. Instead, we want to use the  same type parameter to indicate the presence of _same_ type of data. 


## Type-Classes of Kind `* -> *`

### Functor

| Type          | Method Name  | 
| ------------- |--------------|
| `F[-] => F[Unit]`  | `void`   | 
| `F[A] => B => F[B]`  | `as`   | 
| `F[A] => (A => B) => F[B]` | `map`   | 
| `F[A] => B => F[(B, A)]`  | `tupleLeft`  |
| `F[A] => B => F[(A, B)]`  | `tupleRight` |
| `(A => B) => (F[A] => F[B])` | `lift`   | 

### Apply

| Type          | Method Name | Symbol   |
| ------------- |--------------|------------|
| `F[A] => F[-] => F[A]` | `productL`  | `<*`
| `F[-] => F[B] => F[B]` | `productR`  | `*>` 
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
| `F[A] => (A => F[-]) => F[A]` | `flatTap`


### FunctorFilter

| Type               | Method Name     |
| -------------      | --------------- |
| `F[A] => (A => Boolean) => F[A]` |   `filter` |
| `F[A] => (A => Option[B]) => F[B]` | `mapFilter` |


### `ApplicativeError[E, F]`

The source code of `cats` uses the `E` type variable for the error type. 

| Type          | Method Name  | 
| ------------- |--------------|
| `E => F[A]`   | `raiseError`   |
| `F[A] => F[Either[E,A]]`    | `attempt`     |
| `F[A] => (E => A) => F[A]`  | `handleError`   |
| `F[A] => (E => F[A]) => F[A]` | `handleErrorWith`  |
| `F[A] => (E => A) => F[A]` | `recover`  | 
| `F[A] => (E => F[A]) => F[A]` | `recoverWith`  | 
| `F[A] => (E => F[Unit]) => F[A]` | `onError`  | 
| `Either[E,A] => F[A]` | `fromEither` | 
| `Option[A] => E => F[A]` | `liftFromOption` | 

### `MonadError[E, F]`

| Type          | Method Name  |
| ------------- |--------------|
| `F[A] => E => (A => Boolean) => F[A]` | `ensure`  
| `F[A] => (A => E) => (A => Boolean) => F[A]` | `ensureOr` 
| `F[A] => (E => E) => F[A]`  | `adaptError` 
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

| Type          | Method Name  | Constrains 
| ------------- |--------------|-----------
| `F[A] => B => ((B,A) => B) => F[B]` | `foldLeft` 
| `F[A] => (A => G[B]) => G[B]` | `foldMapM` | `G: Monad` and `B: Monad`

### Reducible 

| Type          | Method Name  | Constrains 
| ------------- |--------------|-----------
| `F[A] => ((A,A) => A) => A` | `reduceLeft`  | 
| `F[A] => A` | `reduce`  | `A: Semigroup`

### Traversable

| Type          | Method Name  | Constrains 
| ------------- |--------------|-----------
| `F[G[A]] => G[F[A]]`  | `sequence` | `G: Applicative` |
| `F[A] => (A => G[B]) => G[F[B]]` | `traverse` | `G: Applicative` |
| `F[A] => (A => G[F[B]]) => G[F[B]]` | `flatTraverse` | `F: FlatMap` and `G: Applicative`
| `F[G[F[A]]] => G[F[A]]` | `flatSequence` | `G: Applicative` and `F: FlatMap` 
| `F[A] => F[(A,Int)]` | `zipWithIndex` | 
| `F[A] => ((A,Int) => B) => F[B]` | `mapWithIndex` | 


## Type Classes of Kind `(*,*) => *`

### Bifunctor

| Type          | Method Name  |
| ------------- |--------------|
| `P[A,B] => (A => C) => P[C, B]` | `leftMap` |
| `P[A,B] => (B => D) => P[A, D]` | `.rightFunctor` and `.map` |
| `P[A,B] => (A => C) => (B => D) => P[C, D]` | `bimap` |

### Compose - Category - Arrow

| Type          | Method Name  | Symbol | Type-Class
--------| ------------- |--------------|-------------
| `F[A, B] => (B => C) => F[A, C]` | `rmap` 
| `F[A, B] => (C => A) => F[C, B]` | `lmap` 
| `F[A, B] => (C => A) => (B => D) => F[C, D]` | `dimap` 
| `F[A, B] => F[C, A] => F[C, B]` | `compose` | `<<<`
| `F[A, B] => F[B, C] => F[A, C]` | `andThen` | `>>>`
| `F[A, B] => F[(A, C), (B, C)]`   | `first` 
| `F[A, B] => F[(C, A), (C, B)]`   | `second` 
| `=> F[A, A]`   | `id` 
| `(A => B) => F[A, B]`  | `lift`    | 
| `F[A, B] => F[C, B] => F[ Either[A, C], B]` | `choice` | `|||` 
| `F[A, B] => F[C, D] => F[ Either[A, C], Either[B, D] ]` | `choose` | `+++`
| `F[A, B] => F[ Either[A, C], Either[B, C] ]` | `left` | 
| `F[A, B] => F[ Either[C, A], Either[C, B] ]` | `right` |
| `F[A, B] => F[A, C] => F[A, (B, C)]` | `merge` | `&&&`
 

## Monad Transformers

### Constructors and wrappers

Most monad transformers and data types come down to a 

| Data Type     | is an alias or wrapper of  |
| ------------- |--------------|
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
| ------------- |--------------|-------------|
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
| ------------- |--------------|-------------|
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
| `ET[F, A, B] => ET[F, B, A]` | `swap` | `F: Functor` 
| `ET[F, A, A] => F[A]`        | `merge` 
