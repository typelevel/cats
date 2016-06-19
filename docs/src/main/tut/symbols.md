#Symbols

Below is a list of symbols used in cats.  

The `~>`, `⊥` and `⊤` symbols can be imported with `import cats._`.

All other symbols can be imported with `import cats.implicits._`

A scaladoc generated list is also available on the [Scaladoc symbols page](http://typelevel.org/cats/api/#index.index-_).

| Symbol     | Name                   | Type Class                                                                                  | Definition                             |
| ---------- | ---------------------- | ----------------------------------------------------------------------------------------    |--------------------------------------- |
| `fa |@| fb`| Cartesian builder      | [`Cartesian[F[_]]`]({{ site.sources }}/core/src/main/scala/cats/Cartesian.scala)            | `|@|(fa: F[A])(fb: F[B]): F[(A, B)]`   |
| `fa *> fb` | right apply            | [`Cartesian[F[_]]`]({{ site.sources }}/core/src/main/scala/cats/Cartesian.scala)            | `*>(fa: F[A])(fb: F[B]): F[A]`         |
| `fa <* fb` | left apply             | [`Cartesian[F[_]]`]({{ site.sources }}/core/src/main/scala/cats/Cartesian.scala)            | `<*(fa: F[A])(fb: F[B]): F[B]`         |
| `x === y`  | equals                 | [`Eq[A]`]({{ site.sources }}/kernel/src/main/scala/cats/kernel/Eq.scala)                    | `eqv(x: A, y: A): Boolean`             |
| `x =!= y`  | not equals             | [`Eq[A]`]({{ site.sources }}/kernel/src/main/scala/cats/kernel/Eq.scala)                    | `neqv(x: A, y: A): Boolean`            |
| `fa >>= f` | flatMap                | [`FlatMap[F[_]]`]({{ site.sources }}/core/src/main/scala/cats/FlatMap.scala)                | `flatMap(fa: F[A])(f: A => F[B]): F[B]`|
| `fa >> fb` | followed by            | [`FlatMap[F[_]]`]({{ site.sources }}/core/src/main/scala/cats/FlatMap.scala)                | `followedBy(fa: F[A])(fb: F[B]): F[B]` |
| `x |-| y`  | remove                 | [`Group[A]`]({{ site.sources }}/kernel/src/main/scala/cats/kernel/Group.scala)              | `remove(x: A, y: A): A`                |
| `x > y`    | greater than           | [`PartialOrder[A]`]({{ site.sources }}/kernel/src/main/scala/cats/kernel/PartialOrder.scala)| `gt(x: A, y: A): Boolean`              |
| `x >= y`   | greater than or equal  | [`PartialOrder[A]`]({{ site.sources }}/kernel/src/main/scala/cats/kernel/PartialOrder.scala)| `gteq(x: A, y: A): Boolean`            |
| `x < y`    | less than              | [`PartialOrder[A]`]({{ site.sources }}/kernel/src/main/scala/cats/kernel/PartialOrder.scala)| `lt(x: A, y: A): Boolean`              |
| `x <= y`   | less than or equal     | [`PartialOrder[A]`]({{ site.sources }}/kernel/src/main/scala/cats/kernel/PartialOrder.scala)| `lteq(x: A, y: A): Boolean`            |
| `x |+| y`  | Semigroup combine      | [`Semigroup[A]`]({{ site.sources }}/kernel/src/main/scala/cats/kernel/Semigroup.scala)      | `combine(x: A, y: A): A`               |
| `x <+> y`  | SemigroupK combine     | [`SemigroupK[F[_]]`]({{ site.sources }}/core/src/main/scala/cats/SemigroupK.scala)          | `combineK(x: F[A], y: F[A]): F[A]`     |
| `F ~> G`   | natural transformation | [`FunctionK[F[_], G[_]]`]({{ site.sources }}/core/src/main/scala/cats/arrow/FunctionK.scala)| `FunctionK` alias                      |
| `F :<: G`  | inject                 | [`Inject[F[_], G[_]]`]({{ site.sources }}/free/src/main/scala/cats/free/package.scala)      | `Inject` alias                         |
| `F :≺: G`  | inject                 | [`Inject[F[_], G[_]]`]({{ site.sources }}/free/src/main/scala/cats/free/package.scala)      | `Inject` alias                         |
| `⊥`        | bottom                 | [N/A]({{ site.sources }}/core/src/main/scala/cats/package.scala)                            | `Nothing`                              |
| `⊤`        | top                    | [N/A]({{ site.sources }}/core/src/main/scala/cats/package.scala)                            | `Any`                                  |
