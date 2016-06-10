| Symbol     | Name                   | Typeclass                                                                            | Definition               |
| ---------- | ---------------------- | ---------------------------------------------------------------------------------    |------------------------- |
| `<*>`      | apply                  | [`Apply`](../../../../core/src/main/scala/cats/Apply.scala)                          | TBD                      |
| `fa |@| fb`| Cartesian builder      | [`Cartesian[F[_]]`](../../../../core/src/main/scala/cats/Cartesian.scala)            | TBD                      |
| `fa *> fb` | right apply            | [`Cartesian[F[_]]`](../../../../core/src/main/scala/cats/Cartesian.scala)            | TBD                      |
| `fa <* fb` | left apply             | [`Cartesian[F[_]]`](../../../../core/src/main/scala/cats/Cartesian.scala)            | TBD                      |
| `x === y`  | equals                 | [`Eq[A]`](../../../../kernel/src/main/scala/cats/kernel/Eq.scala)                    | `eqv(x: A, y: A): Boolean`           |
| `x =!= y`  | not equals             | [`Eq[A]`](../../../../kernel/src/main/scala/cats/kernel/Eq.scala)                    | `Eq.neqv(x, y)`          |
| `fa >>= f` | flatMap                | [`FlatMap[F[_]]`](../../../../core/src/main/scala/cats/syntax/flatMap.scala)         | `FlatMap.flatMap(fa)(f)` |
| `fa >> fb` | followed by            | [`FlatMap[F[_]]`](../../../../core/src/main/scala/cats/syntax/flatMap.scala)         | `FlatMap.flatMap(fa)(_ => fb)` |
| `x |-| y`  | remove                 | [`Group[A]`](../../../../kernel/src/main/scala/cats/kernel/Group.scala)              | `Group.remove(x, y)`     |
| `x > y`    | greater than           | [`PartialOrder[A]`](../../../../kernel/src/main/scala/cats/kernel/PartialOrder.scala)| `PartialOrder.gt(x, y)`  |
| `x >= y`   | greater than or equal  | [`PartialOrder[A]`](../../../../kernel/src/main/scala/cats/kernel/PartialOrder.scala)| `PartialOrder.gteq(x, y)`|
| `x < y`    | less than              | [`PartialOrder[A]`](../../../../kernel/src/main/scala/cats/kernel/PartialOrder.scala)| `PartialOrder.lt(x, y)`  |
| `x <= y`   | less than or equal     | [`PartialOrder[A]`](../../../../kernel/src/main/scala/cats/kernel/PartialOrder.scala)| `PartialOrder.lteq(x, y)`|
| `x |+| y`  | semigroup combine      | [`Semigroup[A]`](../../../../kernel/src/main/scala/cats/kernel/Semigroup.scala)      | `Semigroup.combine(x, y)`|
| `x <+> y`  | semigroupk combine     | [`SemigroupK[F[_]]`](../../../../core/src/main/scala/cats/SemigroupK.scala)          | `SemigroupK.combineK(x, y)`|
| `F ~> G`   | natural transformation | [`FunctionK[F[_], G[_]]`](../../../../core/src/main/scala/cats/arrow/FunctionK.scala)| `FunctionK` alias        |
| `⊥`        | bottom                 | [N/A](../../../../core/src/main/scala/cats/package.scala)                            | N/A                      |
| `⊤`        | top                    | [N/A](../../../../core/src/main/scala/cats/package.scala)                            | N/A                      |
