package cats.evidence

/**  A value of IsK[F, G] is proof that F[_] and G[_] are the same.
 * This means that in any context A[_[_] ] we have the equality A[F] === A[B] */
sealed abstract class IsK[F[_], G[_]] private[IsK]() { ab =>
  import IsK._

  def substitute[Alg[_[_]]](fa: Alg[F]): Alg[G]

  final def apply[X](a: F[X]): G[X] = coerce[X](a)

  final def coerce[X](a: F[X]): G[X] = substitute[λ[f[_] => f[X]]](a)

  final def andThen[H[_]](bc: G =~= H): F =~= H = bc.substitute[F =~= *[_]](ab)

  final def compose[E[_]](za: E =~= F): E =~= G = za.andThen(ab)

  final def flip: G =~= F = ab.substitute[*[_] =~= F](refl)

  final def lower[A[_[_]]]: A[F] === A[G] = IsK.lower[A, F, G](ab)

  /** Given `F =~= G` and `I =~= J` we can prove that `T[F, I] === T[G, J]`. */
  final def lower2[T[_[_], _[_]]]: PartiallyAppliedLower2[T] = new PartiallyAppliedLower2[T]
  final class PartiallyAppliedLower2[T[_[_], _[_]]] {
    def apply[I[_], J[_]](ij: I =~= J): T[F, I] === T[G, J] =
      IsK.lower2[T, F, G, I, J](ab, ij)
  }

  /** Given `F =~= G` we can prove that `T[F, ?] =~= T[G, ?]`. */
  final def lift[T[_[_], _]]: T[F, *] =~= T[G, *] = IsK.lift(ab)

  /** Given `F =~= G` and `I =~= J` we can prove that `T[F, I, ?] =~= T[G, J, ?]`. */
  final def lift2[T[_[_], _[_], _]]: PartiallyAppliedLift2[T] = new PartiallyAppliedLift2[T]
  final class PartiallyAppliedLift2[T[_[_], _[_], _]] {
    def apply[I[_], J[_]](ij: I =~= J): T[F, I, *] =~= T[G, J, *] =
      IsK.lift2(ab, ij)
  }

  final def is[A]: F[A] === G[A] = substitute[λ[f[_] => F[A] === f[A]]](Is.refl[F[A]])
}

object IsK {
  private[this] type Any1[α] = α => Any
  private[this] final case class Refl[F[_]]() extends IsK[F, F] {
    def substitute[A[_[_]]](fa: A[F]): A[F] = fa
  }

  def apply[A[_], B[_]](implicit ab: A =~= B): A =~= B = ab

  private[this] val reflAny = new IsK[λ[α => Any], λ[α => Any]] {
    def substitute[Alg[_[_]]](fa: Alg[λ[α => Any]]) = fa
  }

  @inline implicit def refl[F[_]]: F =~= F = reflAny.asInstanceOf[F =~= F]

  /** Given `F =~= G` we can prove that `A[F] === A[G]`. */
  def lower[A[_[_]], F[_], G[_]](ab: F =~= G): A[F] === A[G] = ab.substitute[λ[a[_] => A[F] === A[a]]](Is.refl[A[F]])

  def const[A, B](ab: A === B): λ[x => A] =~= λ[x => B] = {
    type f[a] = λ[x => A] =~= λ[x => a]
    ab.substitute[f](IsK.refl[λ[x => A]])
  }

  /** Given `A =~= B` and `I =~= J` we can prove that `F[A, I] === F[B, J]`. */
  def lower2[F[_[_], _[_]], A[_], B[_], I[_], J[_]]
  (ab: A =~= B, ij: I =~= J): F[A, I] === F[B, J] = {
    type f1[a[_]] = F[A, I] === F[a, I]
    type f2[i[_]] = F[A, I] === F[B, i]
    ij.substitute[f2](ab.substitute[f1](Is.refl))
  }

  /** Given `A =~= B` we can prove that `F[A, *] =~= F[B, *]`. */
  def lift[F[_[_], *], A[_], B[_]](ab: A =~= B): F[A, *] =~= F[B, *] =
    ab.substitute[λ[a[_] => F[A, *] =~= F[a, *]]](refl[F[A, *]])

  /** Given `A =~= B` and `I =~= J` we can prove that `F[A, I, *] =~= F[B, J, *]`. */
  def lift2[F[_[_], _[_], _], A[_], B[_], I[_], J[_]]
  (ab: A =~= B, ij: I =~= J): F[A, I, *] =~= F[B, J, *] = {
    type f1[a[_]] = F[A, I, *] =~= F[a, I, *]
    type f2[i[_]] = F[A, I, *] =~= F[B, i, *]
    ij.substitute[f2](ab.substitute[f1](refl[F[A, I, *]]))
  }

}