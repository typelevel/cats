/*
rule = "scala:fix.v1_0_0.RenameReducibleMethods"
 */
package fix
package to1_0_0

object RenameReducibleMethodsTests {
  import cats.data.NonEmptyList
  import cats.Reducible
  import cats.syntax.reducible._
  import cats.instances.all._

  val ns = NonEmptyList(1, List(2, 3))
  Reducible[NonEmptyList].traverse1_(ns)(Option.apply)
  ns.traverse1_(Option.apply)

  Reducible[NonEmptyList].intercalate1(ns, 0)
  ns.intercalate1(0)

  Reducible[NonEmptyList].sequence1_(ns.map(Option.apply))
  ns.map(Option.apply).sequence1_
}
