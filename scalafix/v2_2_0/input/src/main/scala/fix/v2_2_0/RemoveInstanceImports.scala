/*
rule = "scala:fix.v2_2_0.RemoveInstanceImports"
 */
package fix
package to2_2_0

import cats.Semigroup

object RemoveInstanceImportsTests {
  {
    import cats.instances.option._
    import cats.instances.int._
    Semigroup[Option[Int]].combine(Some(1), Some(2))
  }

  {
    import cats.implicits._
    Semigroup[Option[Int]].combine(Some(1), Some(2))
  }

  {
    import cats.instances.option._
    import cats.instances.int._
    import cats.syntax.semigroup._
    Option(1) |+| Option(2)
  }

  {
    import cats.implicits._
    1.some |+| 2.some
  }
}
