---
layout: docs
title:  "StateT"
section: "data"
source: "cats/core/src/main/scala/cats/data/package.scala"
scaladoc: "#cats.data.StateT"
---
# StateT

`StateT[F[_], S, A]` is a datatype used to define state machine effectful computations that can be easily composed into a larger one.

The type parameters are:
- `F[_]` representing the effect in which the computation is performed.
- `S` representing the underlying state, shared between each step of the state machine.
- `A` representing the return value.

It can be seen as a way to add the capability to manipulate a shared state, to an existing computation in the context of `F`.

## StateT and State

`StateT` is a monad transformer for [State](https://typelevel.org/cats/datatypes/state.html), in particular [State](https://typelevel.org/cats/datatypes/state.html) is defined as a `StateT` with [Eval](https://typelevel.org/cats/datatypes/eval.html) as the effect `F`.
Therefore, `StateT` exposes the same methods of `State`, such as: `modify`, `get` and `set`.
Plus additional methods, that handles effectful computations, eg: `modifyF`, `setF` and `liftF`.

## Example: Hangman Game

In this example we need to:
- Keep a state of the game, with the target word and the guesses made.
- Show the game state.
- Ask the player to input a guess letter until the game is over.

We can model the state as

```scala mdoc:silent

```
