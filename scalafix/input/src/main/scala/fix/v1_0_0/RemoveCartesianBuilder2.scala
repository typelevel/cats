/*
rewrite = "scala:fix.v1_0_0.RemoveCartesianBuilder"
 */
package fix
package to1_0_0

object RemoveCartesianBuilderTests2 {
  import cats.instances.all._
  import cats.syntax.cartesian._
  val o1: Option[Int] = Some(2)
  val o2: Option[Int] = Some(2)
  o1 *> o2
  (o1 |@| o2).map(_ + _)
}

