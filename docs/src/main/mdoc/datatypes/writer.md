---
layout: docs
title:  "Writer"
section: "data"
source: "core/src/main/scala/cats/data/package.scala"
scaladoc: "#cats.data.Writer"
---
# Writer

The `Writer` monad represents computations which produce a stream of
data in addition to the computed values. When executed, it returns a
tuple of both: the stream and the output value. The main features the
`Writer` provides are:
- The flexibility regarding the stream management. It can be modified
in multiple ways. See the [Operations section](#Operations)
- When two functions are composed together, eg using `flatMap`,
both streams will be combined using a `Semigroup`.

## Operations

### Definition

## Example
