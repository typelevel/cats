/*
rule = "scala:fix.v1_0_0.RenameCartesian"
 */
package fix
package to1_0_0

import cats.Cartesian

object RenameCartesianTests {
  import cats.syntax.cartesian._

  def prod[F[_]: Cartesian, A, B](fa: F[A], fb: F[B]): F[(A, B)] = Cartesian[F].product(fa, fb)
}
