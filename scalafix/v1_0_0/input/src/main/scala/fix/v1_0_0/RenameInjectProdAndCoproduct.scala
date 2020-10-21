/*
rule = "scala:fix.v1_0_0.RenameInjectProdAndCoproduct"
 */
package fix
package to1_0_0

import cats.free.Inject
import cats.data.{ Coproduct, Prod }

object RenameInjectProdAndCoproductTests {
  def inject[F[_], G[_]](implicit inj: Inject[F, G]) = ???
  def prod[F[_], G[_], A](implicit prod: Prod[F, G, A]) = ???
  def coprod[F[_], G[_], A](implicit coprod: Coproduct[F, G, A]) = ???
}
