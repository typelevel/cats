package cats
package laws

import cats.functor.Phantom
import cats.syntax.phantom._

/**
 * Laws that must be obeyed by any `cats.functor.Phantom`.
 */
trait PhantomLaws[F[_]] extends ContravariantLaws[F] {
  implicit override def F: Phantom[F]

  def phantomIdentity[A, B](fa: F[A]): IsEq[F[A]] =
    fa.pmap[B].pmap[A] <-> fa

}

object PhantomLaws {
  def apply[F[_]](implicit ev: Phantom[F]): PhantomLaws[F] =
    new PhantomLaws[F] { def F: Phantom[F] = ev }
}
