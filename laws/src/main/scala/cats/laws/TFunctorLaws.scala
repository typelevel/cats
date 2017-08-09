package cats
package laws


import cats.arrow.FunctionK
import syntax.all._
import cats.~>

trait TFunctorLaws[T[_[_], _]]{
  implicit def T: TFunctor[T]

  def covariantIdentity[F[_], A](fg: T[F, A]): IsEq[T[F, A]] =
    fg.mapNT(FunctionK.id[F]) <-> fg

  def covariantComposition[F[_], G[_], H[_], A](fa: T[F, A], f: F ~> G, g: G ~> H): IsEq[T[H, A]] =
    fa.mapNT(f).mapNT(g) <-> fa.mapNT(f andThen g)

}

object TFunctorLaws {
  def apply[T[_[_], _]](implicit ev: TFunctor[T]): TFunctorLaws[T] =
    new TFunctorLaws[T] { def T: TFunctor[T] = ev }
}

