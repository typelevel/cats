/*
rule = "scala:fix.v2_2_0.RemoveInstanceImports"
 */
package fix
package to2_2_0

import cats.Semigroup
import scala.concurrent.Future

object RemoveInstanceImportsTests {
  {
    import cats.instances.option._
    import cats.instances.int._
    Semigroup[Option[Int]].combine(Some(1), Some(2))
  }

  {
    import cats.instances.all._
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

  {
    import cats.instances.future._
    import cats.instances.int._
    import scala.concurrent.ExecutionContext.Implicits.global
    Semigroup[Future[Int]]
  }

  {
    import cats.instances.all._
    import scala.concurrent.ExecutionContext.Implicits.global
    Semigroup[Future[Int]]
  }

  {
    import cats.implicits._
    import scala.concurrent.ExecutionContext.Implicits.global
    Semigroup[Future[Int]]
  }
}
