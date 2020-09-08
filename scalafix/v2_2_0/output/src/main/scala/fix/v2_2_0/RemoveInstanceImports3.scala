package fix
package to2_2_0

import cats._
import cats.syntax.all._

object RemoveInstanceImportsTests3 {

  def doSomethingMonadic[F[_]: Monad](x: F[Int]): F[String] =
    for {
      a <- x
      b <- Monad[F].pure("hi")
      c <- Monad[F].pure("hey")
    } yield a.toString + b + c

}
