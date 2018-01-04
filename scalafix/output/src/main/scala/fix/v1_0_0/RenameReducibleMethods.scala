package fix
package to1_0_0

object RenameReducibleMethodsTests {
  import cats.data.NonEmptyList
  import cats.Reducible
  import cats.syntax.reducible._
  import cats.instances.all._

  val ns = NonEmptyList(1, List(2, 3))
  Reducible[NonEmptyList].nonEmptyTraverse_(ns)(Option.apply)
  ns.nonEmptyTraverse_(Option.apply)

  Reducible[NonEmptyList].nonEmptyIntercalate(ns, 0)
  ns.nonEmptyIntercalate(0)

  Reducible[NonEmptyList].nonEmptySequence_(ns.map(Option.apply))
  ns.map(Option.apply).nonEmptySequence_
}
