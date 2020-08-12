package fix
package to2_2_0

import cats.Semigroup

object RemoveInstanceImportsTests {
  {
    Semigroup[Option[Int]].combine(Some(1), Some(2))
  }

  {
    import cats.implicits._
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
}
