package cats
package laws

import cats.Id
import cats.data.Nested
import cats.syntax.distributive._

trait DistributiveLaws[F[_]] extends FunctorLaws[F] {
  implicit override def F: Distributive[F]

  def cosequenceIdentity[A](fa: F[A]): IsEq[F[A]] = {
    F.cosequence[Id, A](fa) <-> fa
  }

  def cosequenceTwiceIsId[A, M[_]](fma: F[M[A]])(implicit M: Distributive[M]): IsEq[F[M[A]]] = {
    val result = F.cosequence(M.cosequence(fma))
    fma <-> result
  }

  def composition[A, B, C, M[_], N[_]](
    ma: M[A],
    f: A => F[B],
    g: B => N[C]
  )(implicit
    N: Distributive[N],
    M: Functor[M]
  ): IsEq[Nested[F, N, M[C]]] = {
    val rhs = ma.distribute[Nested[F, N, ?], C](a => Nested(F.map(f(a))(g)))
    val lhs = Nested(F.map(ma.distribute(f))(fb => fb.distribute(g)))
    lhs <-> rhs
  }
}

object DistributiveLaws {
  def apply[F[_]](implicit ev: Distributive[F]): DistributiveLaws[F] =
    new DistributiveLaws[F] { def F: Distributive[F] = ev }
}
