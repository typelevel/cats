package fix
package to1_0_0

import cats.InjectK
import cats.data.{ EitherK, Tuple2K }

object RenameInjectProdAndCoproductTests {
  def inject[F[_], G[_]](implicit inj: InjectK[F, G]) = ???
  def prod[F[_], G[_], A](implicit prod: Tuple2K[F, G, A]) = ???
  def coprod[F[_], G[_], A](implicit coprod: EitherK[F, G, A]) = ???
}
