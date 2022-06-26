package fix
package to1_0_0

import cats.Semigroupal

object RenameCartesianTests {
  import cats.syntax.semigroupal._

  def prod[F[_]: Semigroupal, A, B](fa: F[A], fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
}
