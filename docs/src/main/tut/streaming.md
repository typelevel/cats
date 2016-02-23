---
layout: default
title:  "Streaming"
section: "data"
source: "core/src/main/scala/cats/data/Streaming.scala"
scaladoc: "#cats.data.Streaming"
---

# Streaming

The `Streaming` data type provides support for sequences of values
which can be computed on demand. It is immutable (meaning its
contents will never change), and supports optional memoization.

The data type which `Streaming` implements is often called a *stream*.
In fact, `Streaming` is similar to an existing Scala data type called
`Stream` (the name `Streaming` was chosen to avoid a conflict with the
standard library).

Sometimes the `Streaming` documentation will refer to a *stream*. In
these cases, we will use lowercase *stream* as a general term,
distinguished from `Stream` (the specific type from the standard
library) or `Streaming` (the specific type from Cats).

## Introduction

A non-empty `Streaming` instance is structured like a `List`: it has a
*cons* cell containing a single value, as well as a reference to a
tail which will calculate the subsequent values (if any). This means
that adding values to the beginning is very efficient, whereas adding
values to the end is potentially expensive.

The major difference between `List` and `Streaming` is evaluation.
`List` is strict: this means that if you have an instance of `List`
the entire thing has been calculated and constructed in memory for
you to access, even if you never access any of the list's elements.

Unlike `List`, a `Streaming` instance can lazily-compute its tail. This
means that until the tail is needed, it will not be constructed (and
only the part that is accessed will be constructed). This is very
useful for sequences which are potentially large (or infinite) but are
easily computed.

This laziness might remind you of another familiar type: `Iterator`.
Like `Streaming`, `Iterator` also computes values on-demand using a
`.next` method (along with `.hasNext`). However, the resemblance is
only skin deep, since `Iterator` is a mutable data type. This means
that `Iterator` can only be traversed once, it cannot be safely shared,
and is often more difficult to reason about.

By contrast, `Streaming` can be safely shared (and safely accessed from
many threads simultaneously). It can be traversed as many times as
needed, and has all the other advantages of an immutable data
structure.

## Using Streaming

The `Streaming` type does not extend `Seq[_]`, `Iterable[_]`, or any of
the other Scala collection types. Despite this, you can use `Streaming`
in most of the ways you use other Scala collections:

```tut
import cats.data.Streaming

val ns = Streaming(1, 10, 100, 1000)
val xs = ns.filter(_ < 50)
val ys = ns.map(_ * 2 + 1)
val zs = xs ++ ys

zs.toList
```

The first thing you probably noticed was that `.toString` does not give
the complete contents of a `Streaming` instance. This is intentional:
since the contents may be lazy, we won't print the entire stream by
default. Ellipses (`...`) are used to indicate the possibility of more
elements.

Instead, you can pass an argument to `.toString()` which will specify
the maximum number of terms to be evaluated:

```tut
val ns = Streaming(1, 2, 3, 4, 5)
ns.toString(3)
ns.toString(5)
ns.toString(100)
```

In our previous examples we've been using `Streaming#apply` to
construct our instances. This means these instances are not lazy (since
we provided all the elements to the constructor).

However, we can also construct lazy instances:

```tut
import cats.implicits._

def int(n: Int): Int = { println(s"int($n)"); n }
val now = Streaming(int(1), int(2), int(3))
val later = int(1) %:: int(2) %:: int(3) %:: Streaming.empty[Int]
```

Notice the difference between the `now` and `later` instances. In the
*now* case, we print messages for `1`, `2`, and `3`. In the *later*
case, we only print a message for `1`. This indicates that the tail of
the stream (containing `2` and `3`) will be constructed lazily, i.e. it
has not been constructed yet. We can take advantage of this feature to
write interesting definitions:

```tut
lazy val fives: Streaming[Int] = 5 %:: fives
fives.take(5).toList
```

## Fibonaci Sequence

The Fibonacci sequence starts with `0, 1, 1, 2, 3, 5, 8, 13, ...` and
continues on forever: it is an infinite sequence. Each term is
calculated by adding the two previous terms (the starting values `0`
and `1` are fixed).

Since the sequence grows forever, lets set up an unbounded integer type
(`Z`) as well as some useful values:

```tut:silent
type Z = BigInt

val Z0 = BigInt(0)
val Z1 = BigInt(1)
```

Our first implementation will be a simple recursive method. While this
doesn't use `Streaming`, its definition is very close to one a
mathematian might write.

```tut:silent
def fib(n: Int): Z =
  n match {
    case 0 => Z0
    case 1 => Z1
    case x if x > 1 => fib(x - 1) + fib(x - 2)
    case x => sys.error(s"invalid x ($x)")
  }
```

However, we can also model the Fibonacci sequence as an infinite stream
of values, where each value depends upon the two previous values. Our
first implementation uses a `lazy val` to set up a self-referential
structure which captures this relationship.

```tut:silent
lazy val fibs: Streaming[Z] =
  Z0 %:: Z1 %:: (fibs zipMap fibs.drop(1))(_ + _)
fibs.take(6).toList
```

You might be surprised that you can zip a stream with itself. But
because we are dealing with immutable values, there is no problem!
There is no way that referencing the same stream multiple times can go
wrong, or corrupt internal state.

We can also be a bit more explicit about the recursive nature of the
Fibonacci sequence, while still modeling it with `Streaming[Z]`. Since
each "step" of the sequence generates the next term given the current
state (and sets up subsequent steps by changing the state) we can use
an inner method (`term`) which acts exactly the same way:

```tut:silent
val fibs: Streaming[Z] = {
  def term(x: Z, y: Z): Streaming[Z] = x %:: term(y, x + y)
  term(Z0, Z1)
}
```

In this formulation, `x` is the "current" term and `y` is the "next"
term. The term after `y` will be `x + y`, which we calculate after
emitting `x` (and which is saved along with `y` in our suspended call
to `term`).

One thing to keep in mind is that the `%::` syntax defers the call to
`term` (it is interpreted as a by-name parameter through some
syntactic trickery). This means that `term` is not a recursive method
in the usual sense: the method does not call itself directly, but
instead builds a `Streaming[Z]` instance which will potentially call
`term` itself when the stream's tail is needed.

This technique of defining streams (using an inner `term` method) is
quite powerful and general. Our third example uses a built-in method
for producing infinite streams that can be defined in the same way (a
current *state* and a way to get the next *state* from the current
one):

```tut:silent
val fibs: Streaming[Z] =
  Streaming.infinite((Z0, Z1)) { case (x, y) => (y, x + y) } map (_._1)
```

The first argument to `.infinite` is the starting state (`(Z0, Z1)`,
i.e. zero and one). The next argument is a method from the current
state (`(x, y)`) to the next one (`(y, x + y)`). Ignoring the call to
`.map` for right now, this is the sequence that would be produced:

```
start: (0, 1)
step1: (1, 1)
step2: (1, 2)
step3: (2, 3)
step4: (3, 5)
...
```

If you look at the first column of numbers you can see the familiar
Fibonacci sequence. However, our sequence needs to return values of
type `Z`, not tuples of `(Z, Z)` pairs. The call to `map (_._1)` is
just to take the first of the two numbers, to produce the sequence we
want.

If we had a sequence that only relied on one previous number (e.g. `1,
2, 3, ...`) we would not need this step. If we had a sequence that
used a much more complicated state we could model it similarly: create
an infinite stream of states, then map the states to the values we
want to emit.

## Counting the rational numbers

To prove these kinds of constructions work for examples which are more
complex, let's model an infinite stream of all rational numbers. This
infinite stream should generate every rational number (no matter how
big or small) exactly once at some point. (In practice we have limited
time and space, so we will have to make do with the numbers we have
the patience to compute.)

First let's review what a rational number is. A rational number is any
number which can be written as a fraction. For example 3 is a rational
number (3/1), so is 0.2 (1/5), so is 117/113, etc.

To simplify the example, we're going to use the type `(Z, Z)` to
represent a rational number (which we will alias as `Q`). The first
value of the tuple is the numerator, and the second is the
denominator. We'll also limit ourselves to *canonical* rational
numbers: numbers whose denominator is positive, and which are in
simplest form. For example, 2/5, 3/1, and -9/1 are canonical, whereas
6/-5, -1/-1, and 4/10 are not.

```tut:silent
type Q = (Z, Z)

// Some useful rational constants for zero and one.
val Q0 = (Z0, Z1)
val Q1 = (Z1, Z1)
```

The actual code for defining this stream is very concise, but needs
some explanation. The basic approach is that we want to generate all
possible pairs of integers, and then filter out anything that is not
canonical. This will leave us with all the rational numbers:

```tut:silent
val zs = Streaming.infinite(Z1)(_ + Z1)
val pairs = (zs product zs)
val qs = pairs.filter { case (n, d) => (n gcd d) == Z1 }
val rats = Q0 %:: qs.flatMap { case q @ (n, d) => Streaming(q, (-n, d)) }
```

First we define `zs`, an infinte stream of all the positive numbers
(beginning with one). Next we use the `product` method to create the
Cartesian product of `zs` with itself (every possible pairing of two
values from `zs`). Here are the first few terms of `pairs` to give you
the idea:

```tut
pairs.toString(12)
```

As you can see, this sequence contains some pairs which are not
canonical rational numbers (e.g. `(2, 2)`). The next step filters
these out using the `gcd` method (which finds the greatest common
divisor between two numbers). Unless the GCD is one, the numbers share
a divisor, and are not in the simplest form. In the case of `(2, 2)`,
the simplest form would be `(1, 1)` which we already saw. (In fact,
every non-canonical rational number has a canonical equivalent which
will have already appeared in the stream.) Here are the first few
terms of `qs`:

```tut
qs.toString(12)
```

Finally, we have to do two things to produce a correct
sequence. First, we have to include zero (since we started `zs` with
one, we won't have any zeros in `qs`). Second, we need to produce
negative numbers (the values in `qs` are all positive). Once we've
accomplished these things, we're done. Here are the first few terms of `rats`:

```tut
rats.toString(12)
```

## pseudo-random numbers

We can also use `Streaming` to implement streams of pseudo-random
values. Every pseudo-random number generator (PRNG) is a combination
of a state value, and a transition function to produce the next state
from the current one. This is just like our Fibonacci example from
earlier. We will use the same PRNG we do in the documentation for
`StateT`, a linear-congruent generator devised by Donald Knuth call
MMIX.

The basic idea behind the PRNG is that multiplying two large `Long`
values will cause the value to "wrap around" to another valid `Long`
value. Using addition and multiplication, we can create a stream of
`Long` values which appear random:

```tut
def createMmix(seed: Long): Streaming[Long] =
  Streaming.infinite(seed)(n => n * 6364136223846793005L + 1442695040888963407L)

val longs: Streaming[Long] = createMmix(0x741f2d2eea7e5c70L)

longs.toString(5)
```

Our stream (`longs`) produces an infinite sequence of arbitrary `Long`
values, based on the seed (`0x741f2d2eea7e5c70`) which was provided to
`createMmix`.

We can transform the `longs` stream into any kind of stream we
want. For example, if we want a `Streaming[Int]` instead we can split
each `Long` value in two. We'll use `.flatMap` to produce a
`Streaming[Int]` containing two 32-bit `Int` values for each 64-bit
`Long`:

```tut
val ints: Streaming[Int] =
  longs.flatMap { (n: Long) =>
    val n0 = ((n >>> 32) & 0xffffffff).toInt
    val n1 = (n & 0xffffffff).toInt
    Streaming(n0, n1)
  }
```

It's worth noting that while `longs` is infinite, there are only a
finite number of `Long` values. More worryingly, the same input value
will also produce the same output value, meaning that eventually our
sequence will hit a cycle and repeat itself. (In the case of MMIX the
period of the sequence is 2^64.)

PRNGs which need to provide a large amount of random entropy will use
much larger state variables, and have correspondingly larger periods.

## Digits of Pi

The final example is more complex than the previous examples: we will
compute the digits of Pi as a `Streaming[Z]`, where each digit is one
element in the stream. Since Pi has an infinite decimal expansion, the
stream of digits is also infinite.

The algorithm used is ported from this [python code](http://www.cs.utsa.edu/~wagner/pi/pi_cont.html).
It uses a [continued fraction](https://en.wikipedia.org/wiki/Continued_fraction)
to get ever-closer approximations of Pi, which it uses to produce decimal digits.

First we will need to define some more numerical constants:

```tut:silent
val Z2 = BigInt(2)
val Z4 = BigInt(4)
val Z10 = BigInt(10)
val Z12 = BigInt(12)
```

To make the algorithms' structure a bit more amenable to defining a
stream, we are going to break it up into two parts:

 * `narrow`: the outer loop, refines the approximation of Pi.
 * `emit`: the inner loop, outputs decimal terms as available.

You might notice that these methods are corecursive (they call each
other). In some cases, corecursive methods will lead to stack
overflows (if the call chain becomes too "deep"). However, the details
of this algorithms ensure that there won't be very many `narrow` and
`emit` calls before a term of the stream is output.

Also, since we are outputting decimal digits (0-9) you might wonder
why our calculations are done using `Z`, rather than the much less
expensive `Int` which we return. The reason we do this is that
internally our calculation needs to use very large numbers to
represent increasingly precise fractional approximations of Pi.

```tut:silent
val pi: Streaming[Int] = {

  def narrow(k: Z, a: Z, b: Z, a1: Z, b1: Z): Streaming[Int] = {
    val (p, q) = (k * k, k * Z2 + Z1)
    val (aa, bb, aa1, bb1) = (a1, b1, p * a + q * a1, p * b + q * b1)
    val (d, d1) = (aa / bb, aa1 / bb1)
    emit(k + Z1, aa, bb, aa1, bb1, p, q, d, d1)
  }

  def emit(k: Z, a: Z, b: Z, a1: Z, b1: Z, p: Z, q: Z, d: Z, d1: Z): Streaming[Int] =
    if (d != d1) narrow(k, a, b, a1, b1) else {
      val (aa, aa1) = ((a % b) * Z10, (a1 % b1) * Z10)
      val (dd, dd1) = (aa / b, aa1 / b1)
      d.toInt %:: emit(k, aa, b, aa1, b1, p, q, dd, dd1)
    }

  // Starting parameters needed to calculate Pi.
  narrow(Z2, Z4, Z1, Z12, Z4)
}
```

Once again, we have our starting state (the five inputs to `narrow`)
and our transition function (`narrow` and `emit` in tandem), so we can
build a stream.

Does it work? Let's find out!

```tut
def str(xs: Streaming[Int]): String =
  xs.foldLeft("")(_ + _.toString)

val h = pi.take(1)
val t = pi.drop(1).take(40)
str(h) + "." + str(t) + "..."
```

## Conclusion

Lazy, immutable streams are a powerful way to model an in-progress
calculation, especially when those sequences are potentially
unbounded. While these examples were based on mathematical problems,
streams are a great way to model any case where waiting to collect all
the elements of a sequence would be inefficient or prohibitive.
