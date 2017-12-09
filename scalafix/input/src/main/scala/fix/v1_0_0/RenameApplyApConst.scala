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
  Apply[NonEmptyList].forEffect(l)(r)
  Apply[NonEmptyList].followedBy(l)(r)

  l forEffect r
  l followedBy r

  NonEmptyParallel[NonEmptyList, ZipNonEmptyList].parForEffect(l)(r)
  NonEmptyParallel[NonEmptyList, ZipNonEmptyList].parFollowedBy(l)(r)

  l parForEffect r
  l parFollowedBy r

  FlatMap[NonEmptyList].forEffectEval(l)(Eval.now(r))
  FlatMap[NonEmptyList].followedByEval(l)(Eval.now(r))

  l forEffectEval Eval.now(r)
  l followedByEval Eval.now(r)
}
