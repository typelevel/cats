# Traverse

API Documentation: @:api(cats.Traverse)

In the `Applicative` tutorial we saw a more polymorphic version of the standard library
`Future.traverse` and `Future.sequence` functions, generalizing `Future` to be any
`F[_]` that's `Applicative`.

```scala mdoc:silent
import cats.Applicative

def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
  as.foldRight(Applicative[F].pure(List.empty[B])) { (a: A, acc: F[List[B]]) =>
    val fb: F[B] = f(a)
    Applicative[F].map2(fb, acc)(_ :: _)
  }
```

Here `traverse` still has knowledge of `List`, but we could just as easily use
`Vector` or some similar data type. Another example is a binary tree:

```scala mdoc:silent
object tree {
  sealed abstract class Tree[A] extends Product with Serializable {
    def traverse[F[_]: Applicative, B](f: A => F[B]): F[Tree[B]] = this match {
      case Tree.Empty()         => Applicative[F].pure(Tree.Empty())
      case Tree.Branch(v, l, r) => Applicative[F].map3(f(v), l.traverse(f), r.traverse(f))(Tree.Branch(_, _, _))
    }
  }

  object Tree {
    final case class Empty[A]() extends Tree[A]
    final case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
  }
}

import tree._
```

This suggests an abstraction over "things that can be traversed over," hence `Traverse`.

```scala mdoc:silent
trait Traverse[F[_]] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

// Example implementation for List
implicit val traverseForList: Traverse[List] = new Traverse[List] {
  def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
    fa.foldRight(Applicative[G].pure(List.empty[B])) { (a, acc) =>
      Applicative[G].map2(f(a), acc)(_ :: _)
    }
}

// Example implementation for Tree
implicit val traverseForTree: Traverse[Tree] = new Traverse[Tree] {
  def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] =
    fa.traverse(f)
}
```

## A note on sequencing

Sometimes you will be given a traversable that has effectful values already, such as
a `List[Option[A]]`. Since the values themselves are effects, traversing with `identity`
will turn the traversable "inside out."

```scala mdoc:reset:silent
import cats.syntax.all._
```

```scala mdoc
val list = List(Some(1), Some(2), None)
val traversed = list.traverse(identity)
```

Cats provides a convenience method for this called `sequence`.

```scala mdoc
val sequenced = list.sequence
```

In general `t.map(f).sequence` can be replaced with `t.traverse(f)`.

## Traversables are Functors

As it turns out every `Traverse` is a lawful `Functor`. By carefully picking the `G` to
use in `traverse` we can implement `map`.

First let's look at the two signatures.

```scala mdoc:silent
import cats.{Applicative, Traverse}

def traverse[F[_]: Traverse, G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = ???

def map[F[_]: Traverse, A, B](fa: F[A])(f: A => B): F[B] = ???
```

Both have an `F[A]` parameter and a similar `f` parameter. `traverse` expects the return type
of `f` to be `G[B]` whereas `map` just wants `B`. Similarly the return type of `traverse` is
`G[F[B]]` whereas for `map` it's just `F[B]`. This suggests we need to pick a `G` such that
`G[A]` communicates exactly as much information as `A`. We can conjure one up by simply wrapping
an `A`.

```scala mdoc:silent
final case class Id[A](value: A)
```

In order to call `traverse` `Id` needs to be `Applicative` which is straightforward - note that while
`Id` just wraps an `A`, it is still a type constructor which matches the shape required by `Applicative`.

```scala mdoc:silent
implicit val applicativeForId: Applicative[Id] = new Applicative[Id] {
  def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = Id(ff.value(fa.value))

  def pure[A](a: A): Id[A] = Id(a)
}
```

Now we can implement `map` by wrapping and unwrapping `Id` as necessary.

```scala mdoc:silent
import cats.Functor

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Id(f(a))).value
}
```

`Id` is provided in Cats as a type alias `type Id[A] = A`.

## Traversables are Foldable

The `Foldable` type class abstracts over "things that can be folded over" similar to how
`Traverse` abstracts over "things that can be traversed." It turns out `Traverse` is strictly
more powerful than `Foldable` - that is, `foldLeft` and `foldRight` can be implemented
in terms of `traverse` by picking the right `Applicative`. However, `cats.Traverse` does not
implement `foldLeft` and `foldRight` as the actual implementation tends to be ineffecient.

For brevity and demonstration purposes we'll implement the equivalent `foldMap` method in terms
of `traverse` by using `cats.data.Const`. You can then implement `foldRight` in terms of `foldMap`,
and `foldLeft` can then be implemented in terms of `foldRight`, though the resulting implementations
may be slow.

```scala mdoc:reset:silent
import cats.{Applicative, Monoid, Traverse}
import cats.data.Const

def foldMap[F[_]: Traverse, A, B: Monoid](fa: F[A])(f: A => B): B =
  Traverse[F].traverse[Const[B, *], A, B](fa)(a => Const(f(a))).getConst
```

This works because `Const[B, *]` is an `Applicative` if `B` is a `Monoid`, as explained in [the documentation of `Const`](../datatypes/const.md#example-2-traverse).

## Further Reading

* [The Essence of the Iterator Pattern][iterator] - Gibbons, Oliveira. JFP 2009.

[iterator]: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf "The Essence of the Iterator Pattern"
