| Symbol     | Name                   | Typeclass                                                             | Definition               |
| ---------- | ---------------------- | --------------------------------------------------------------------- |------------------------- |
| `<*>`      | apply                  | [`Apply`](core/src/main/scala/cats/Apply.scala)                       | TBD                      |
| `|@|`      | Cartesian builder      | [`Cartesian[F[_]]`](core/src/main/scala/cats/Cartesian.scala)         | TBD                      |
| `fa *> fb` | right apply            | [`Cartesian[F[_]]`](core/src/main/scala/cats/Cartesian.scala)         | TBD                      |
| `fa <* fb` | left apply             | [`Cartesian`](core/src/main/scala/cats/Cartesian.scala)               | TBD                      |
| `x === y`  | equals                 | [`Eq[A]`](kernel/src/main/scala/cats/kernel/Eq.scala)                 | `Eq.eqv(x, y)`           |
| `x =!= y`  | not equals             | [`Eq[A]`](kernel/src/main/scala/cats/kernel/Eq.scala)                 | `Eq.neqv(x, y)`          |
| `fa >>= f` | flatMap                | [`FlatMap[F[_]]`](core/src/main/scala/cats/syntax/flatMap.scala)      | `FlatMap.flatMap(fa)(f)` |
| `fa >> fb` | followed by            | [`FlatMap[F[_]]`](core/src/main/scala/cats/syntax/flatMap.scala)      | `FlatMap.flatMap(fa)(_ => fb)` |
| `|-|`      | remove                 | [`Group[A]`](kernel/src/main/scala/cats/kernel/Group.scala)           | `Group.remove(a, b)`     |
| `>`        | greater than           | [`PartialOrder`](kernel/src/main/scala/cats/kernel/PartialOrder.scala)| TBD |
| `>=`       | greater than or equal  | [`PartialOrder`](kernel/src/main/scala/cats/kernel/PartialOrder.scala)| TBD |
| `<`        | less than              | [`PartialOrder`](kernel/src/main/scala/cats/kernel/PartialOrder.scala)| TBD |
| `<=`       | less than or equal     | [`PartialOrder`](kernel/src/main/scala/cats/kernel/PartialOrder.scala)| TBD |
| `|+|`      | plus                   | [`Semigroup`](kernel/src/main/scala/cats/kernel/Semigroup.scala)      | TBD |
| `<+>`      | combine                | [`SemigroupK`](core/src/main/scala/cats/SemigroupK.scala)             | TBD |
| `~>`       | natural transformation | [`FunctionK`](core/src/main/scala/cats/arrow/FunctionK.scala)         | TBD |
| `⊥`        | bottom                 | [N/A](core/src/main/scala/cats/package.scala)                         | TBD |
| `⊤`        | top                    | [N/A](core/src/main/scala/cats/package.scala)                         | TBD |
