package fix
package to2_2_0

import cats.Semigroup
import scala.concurrent.Future

object RemoveInstanceImportsTests {
  {
    Semigroup[Option[Int]].combine(Some(1), Some(2))
  }

  {
    Semigroup[Option[Int]].combine(Some(1), Some(2))
  }

  {
    Semigroup[Option[Int]].combine(Some(1), Some(2))
  }

  {
    import cats.syntax.semigroup._
    Option(1) |+| Option(2)
  }

  {
    import cats.implicits._
    1.some |+| 2.some
  }

  {
    import scala.concurrent.ExecutionContext.Implicits.global
    Semigroup[Future[Int]]
  }

  {
    import scala.concurrent.ExecutionContext.Implicits.global
    Semigroup[Future[Int]]
  }

  {
    import scala.concurrent.ExecutionContext.Implicits.global
    Semigroup[Future[Int]]
  }
}
