/*
rule = "scala:fix.v1_0_0.ContraMapToLMap"
 */
package fix
package to1_0_0



object ContraMapToLMapTests {
  import cats.Show
  import cats.syntax.all._
  import cats.instances.all._

  val f: Int => String = _.toString

  val g: Int => String = f.contramap(_ + 1)

  object Foo
  object Bar
  object Baz

  implicit val showFoo: Show[Foo.type] = Show.fromToString

  val showBar: Show[Bar.type] = showFoo.contramap(_ => Foo)

  val showBaz: Show[Baz.type] = Show[Foo.type].contramap(_ => Foo)

  def getShowBar(): Show[Bar.type] = showBar

  getShowBar().contramap((_: Int) => Bar)
}
