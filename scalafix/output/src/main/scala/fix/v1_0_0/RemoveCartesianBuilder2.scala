package fix
package to1_0_0

object RemoveCartesianBuilderTests2 {
  import cats.instances.all._
  import cats.syntax.cartesian._
  import cats.syntax.apply._
  val o1: Option[Int] = Some(2)
  val o2: Option[Int] = Some(2)
  o1 *> o2
  (o1, o2).mapN(_ + _)
}

