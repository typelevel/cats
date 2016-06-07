| Symbol | Name                   | Typeclass      | Import                       | Source                                              |
| ------ | ---------------------- | ------------   | --------------------------   | -------------------------------------------------   |
| `<*>`  | apply                  | `Apply`        | TBA                          | core/src/main/scala/cats/Apply.scala                |
| `|@|`  | Cartesian builder      | `Cartesian`    | `cats.syntax.cartesian._`    | core/src/main/scala/cats/Cartesian.scala            |
| `*>`   | right apply            | `Cartesian`    | `cats.syntax.cartesian._`    | core/src/main/scala/cats/Cartesian.scala            |
| `<*`   | left apply             | `Cartesian`    | `cats.syntax.cartesian._`    | core/src/main/scala/cats/Cartesian.scala            |
| `===`  | equals                 | `Eq`           | `cats.syntax.eq._`           | kernel/src/main/scala/cats/kernel/Eq.scala          |
| `=!=`  | not equals             | `Eq`           | `cats.syntax.eq._`           | kernel/src/main/scala/cats/kernel/Eq.scala          |
| `>>=`  | flatMap                | `FlatMap`      | `cats.syntax.flatMap._`      | core/src/main/scala/cats/FlatMap.scala              |
| `|-|`  | remove                 | `Group`        | `cats.syntax.group._`        | kernel/src/main/scala/cats/kernel/Group.scala       |
| `>`    | greater than           | `PartialOrder` | `cats.syntax.partialOrder._` | kernel/src/main/scala/cats/kernel/PartialOrder.scala|
| `>=`   | greater than or equal  | `PartialOrder` | `cats.syntax.partialOrder._` | kernel/src/main/scala/cats/kernel/PartialOrder.scala|
| `<`    | less than              | `PartialOrder` | `cats.syntax.partialOrder._` | kernel/src/main/scala/cats/kernel/PartialOrder.scala|
| `<=`   | less than or equal     | `PartialOrder` | `cats.syntax.partialOrder._` | kernel/src/main/scala/cats/kernel/PartialOrder.scala|
| `|+|`  | plus                   | `Semigroup`    | `cats.syntax.semigroup._`    | kernel/src/main/scala/cats/kernel/Semigroup.scala   |
| `<+>`  | combine                | `SemigroupK`   | `cats.syntax.semigroupk._`   | core/src/main/scala/cats/SemigroupK.scala           |
| `~>`   | natural transformation | `FunctionK`    | `cats._`                     | core/src/main/scala/cats/arrow/FunctionK.scala      |
| `⊥`    | bottom                 |  N/A           | `cats._`                     | core/src/main/scala/cats/package.scala              |
| `⊤`    | top                    |  N/A           | `cats._`                     | core/src/main/scala/cats/package.scala              |