package cats.evidence

/**
 * A value of IsK2[F, G] is proof that F[_,_] and G[_,_] are the same.
 * This means that in any context A[_[_,_] ] we have the equality A[F] === A[B]
 */
sealed abstract class IsK2[F[_, _], G[_, _]] private[IsK2] () { ab =>
  import IsK2._

  def subst[Alg[_[_, _]]](fa: Alg[F]): Alg[G]

  final def apply[X, Y](a: F[X, Y]): G[X, Y] = coerce[X, Y](a)

  final def coerce[X, Y](a: F[X, Y]): G[X, Y] = {
    type L[f[_, _]] = f[X, Y]
    subst[L](a)
  }

  final def andThen[H[_, _]](bc: G =~~= H): F =~~= H = {
    type L[f[_, _]] = F =~~= f
    bc.subst[L](ab)
  }

  final def compose[E[_, _]](za: E =~~= F): E =~~= G = za.andThen(ab)

  final def flip: G =~~= F = {
    type L[f[_, _]] = f =~~= F
    ab.subst[*[_, _] =~~= F](refl)
  }

  final def lower[A[_[_, _]]]: A[F] === A[G] = IsK2.lower[A, F, G](ab)

  final def is[A, B]: F[A, B] === G[A, B] = subst[λ[f[_, _] => F[A, B] === f[A, B]]](Is.refl[F[A, B]])
}

object IsK2 {
  private[this] val reflAny = new IsK2[λ[(a, b) => Any], λ[(a, b) => Any]] {
    def subst[Alg[_[_, _]]](fa: Alg[λ[(a, b) => Any]]) = fa
  }

  @inline implicit def refl[A[_, _]]: A =~~= A = reflAny.asInstanceOf[A =~~= A]

  def apply[A[_, _], B[_, _]](implicit ab: A =~~= B): A =~~= B = ab

  /** Given `F =~= G` we can prove that `A[F] === A[G]`. */
  def lower[A[_[_, _]], F[_, _], G[_, _]](ab: F =~~= G): A[F] === A[G] =
    ab.subst[λ[a[_, _] => A[F] === A[a]]](Is.refl[A[F]])

}
