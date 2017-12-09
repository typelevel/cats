package cats
package laws

import cats.Id
import cats.data.Nested
import cats.syntax.distributive._

trait DistributiveLaws[F[_]] extends FunctorLaws[F] {
  implicit override def F: Distributive[F]

  def cosequenceIdentity[G[_], A](fa: F[A]): IsEq[F[A]] = {
    F.cosequence[Id, A](fa) <-> fa
  }

  def cosequenceTwiceIsId[A, M[_]](
    fma: F[M[A]],
  )(implicit
    M: Distributive[M]
  ): IsEq[F[M[A]]] = {
    val result = F.cosequence(M.cosequence(fma))
    fma <-> result
  }

  def composition[A, B, C, M[_], N[_]](
    fa: F[A],
    f: A => M[B],
    g: B => N[C]
  )(implicit
    N: Distributive[N],
    M: Distributive[M]
  ): IsEq[Nested[M, N, F[C]]] = {
    val rhs = fa.distribute[Nested[M, N, ?], C](a => Nested(M.map(f(a))(g)))
    val lhs = Nested(M.map(fa.distribute(f))(fb => fb.distribute(g)))
    lhs <-> rhs    
  }
}

object DistributiveLaws {
  def apply[F[_]](implicit ev: Distributive[F]): DistributiveLaws[F] =
    new DistributiveLaws[F] { def F: Distributive[F] = ev }
}
