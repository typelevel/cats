| Symbol     | Name                   | Typeclass                                                             | Definition |
| ---------------- | ---------------------- | --------------------------------------------------------------------- |----------- |
| `<*>`      | apply                  | [`Apply`](core/src/main/scala/cats/Apply.scala)                       | TBD        |
| `|@|`      | Cartesian builder      | [`Cartesian[F[_]]`](core/src/main/scala/cats/Cartesian.scala)               | TBD        |
| `fa *> fb` | right apply            | [`Cartesian[F[_]]`](core/src/main/scala/cats/Cartesian.scala)               | `Functor.map(Cartesian.product(fa, fb)) { case (a, b) => b }` |
| `fa <* fb` | left apply             | [`Cartesian`](core/src/main/scala/cats/Cartesian.scala)               | `Functor.map(Cartesian.product(fa, fb)) { case (a, b) => a }` |
| `x === y`  | equals                 | [`Eq`](kernel/src/main/scala/cats/kernel/Eq.scala)                    | `Eq[A].eqv(x, y)`  |
| `x =!= y`  | not equals             | [`Eq`](kernel/src/main/scala/cats/kernel/Eq.scala)                    | `Eq[A].neqv(x, y)` |
| `fa >>= f` | flatMap                | [`FlatMap`](core/src/main/scala/cats/syntax/flatMap.scala)            | `FlatMap[A, B].flatMap(fa)(f)` |
| `|-|`      | remove                 | [`Group`](kernel/src/main/scala/cats/kernel/Group.scala)              | TBD |
| `>`        | greater than           | [`PartialOrder`](kernel/src/main/scala/cats/kernel/PartialOrder.scala)| TBD |
| `>=`       | greater than or equal  | [`PartialOrder`](kernel/src/main/scala/cats/kernel/PartialOrder.scala)| TBD |
| `<`        | less than              | [`PartialOrder`](kernel/src/main/scala/cats/kernel/PartialOrder.scala)| TBD |
| `<=`       | less than or equal     | [`PartialOrder`](kernel/src/main/scala/cats/kernel/PartialOrder.scala)| TBD |
| `|+|`      | plus                   | [`Semigroup`](kernel/src/main/scala/cats/kernel/Semigroup.scala)      | TBD |
| `<+>`      | combine                | [`SemigroupK`](core/src/main/scala/cats/SemigroupK.scala)             | TBD |
| `~>`       | natural transformation | [`FunctionK`](core/src/main/scala/cats/arrow/FunctionK.scala)         | TBD |
| `⊥`        | bottom                 | [N/A](core/src/main/scala/cats/package.scala)                         | TBD |
| `⊤`        | top                    | [N/A](core/src/main/scala/cats/package.scala)                         | TBD |
