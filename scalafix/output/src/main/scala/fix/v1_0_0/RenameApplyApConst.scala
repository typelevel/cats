/*
rule = "scala:fix.v1_0_0.RenameReducibleMethods"
 */
package fix
package to1_0_0


object RenameApplyApConst {
  import cats.{NonEmptyParallel, FlatMap, Apply, Eval}
  import cats.data.NonEmptyList
  import cats.data.NonEmptyList.ZipNonEmptyList
  import cats.instances.all._
  import cats.syntax.all._

  val l = NonEmptyList.of(5, 10)
  val r = NonEmptyList.of(12, 13)
  Apply[NonEmptyList].apL(l)(r)
  Apply[NonEmptyList].apR(l)(r)

  l apL r
  l apR r

  NonEmptyParallel[NonEmptyList, ZipNonEmptyList].parApL(l)(r)
  NonEmptyParallel[NonEmptyList, ZipNonEmptyList].parApR(l)(r)

  l parApL r
  l parApR r

  FlatMap[NonEmptyList].apLEval(l)(Eval.now(r))
  FlatMap[NonEmptyList].apREval(l)(Eval.now(r))

  l apLEval Eval.now(r)
  l apREval Eval.now(r)
}
