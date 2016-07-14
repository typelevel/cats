---
layout: default
title:  "MonoidK"
section: "typeclasses"
source: "core/src/main/scala/cats/MonoidK.scala"
scaladoc: "#cats.MonoidK"
---
# MonoidK

`MonoidK` is a universal monoid which operates on kinds.
 
This type class is useful when its type parameter `F[_]` has a
structure that can be combined for any particular type, and which
also has an "empty" representation. Thus, `MonoidK` is like a `Monoid`
for kinds (i.e. parameterized types).

A `MonoidK[F]` can produce a `Monoid[F[A]]` for any type `A`.

Here's how to distinguish `Monoid` and `MonoidK`:

  - `Monoid[A]` allows `A` values to be combined, and also means there
    is an "empty" A value that functions as an identity.

  - `MonoidK[F]` allows two `F[A]` values to be combined, for any `A`.  It
    also means that for any `A`, there is an "empty" `F[A]` value. The
    combination operation and empty value just depend on the
    structure of `F`, but not on the structure of `A`.
