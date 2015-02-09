package cats
package free

/** 
 * Codensity Monad
 * 
 * Codensity is used to improve quadratic complexity of free monadic structures.
 * This approach is presented in this paper:
 * http://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf
 *
 * The idea behind Codensity is simply to replace left associated structures by wrapping into right associated codensity.
 * There is an isomorphism between Free monadic structure and codensity monad making it lossless to be used in your code.
 *
 * Interesting aspects to know:  
 * - Codensity[F[_], A] = NaturalTransformation[λ[α => Kleisli[F, A, α]], F]
 * - For any type constructor F[_], Codensity is a monad
 */
trait Codensity[F[_], A] {
  cod =>

  def apply[B](f: A => F[B]): F[B]

  def flatMap[B](f: A => Codensity[F, B]): Codensity[F, B] = new Codensity[F, B] {
    def apply[C](g: B => F[C]): F[C] = cod.apply(a => f(a).apply(g))
  }

  def map[B](f: A => B): Codensity[F, B] = flatMap(a => Codensity.pure(f(a)))

  def lower(implicit F: Applicative[F]): F[A] = Codensity.abs(cod)(F)

}

object Codensity {

  def pure[F[_], A](a: => A): Codensity[F, A] = new Codensity[F, A] {
    def apply[B](f: A => F[B]): F[B] = f(a) 
  }

  /** abs is the function defined in paper */
  @inline def abs[F[_], A](ca: Codensity[F, A])(implicit F: Applicative[F]): F[A] = ca.apply(a => F.pure(a))

  /** rep is the function defined in paper */
  @inline def rep[M[_], A](ma: M[A])(implicit M: FlatMap[M]): Codensity[M, A] = new Codensity[M, A] {
    def apply[B](f: A => M[B]): M[B] = M.flatMap(ma)(f)
  }

  /** an alias for rep that can mean something more to you */
  def liftM[M[_], A](ma: M[A])(implicit M: FlatMap[M]): Codensity[M, A] = rep(ma)

  /** TODO Where to put those instances ??? */
  implicit def codensityMonad[F[_]]: Monad[Codensity[F, ?]] =
    new Monad[Codensity[F, ?]] {
      def flatMap[A, B](fa: Codensity[F, A])(f: A => Codensity[F, B]): Codensity[F, B] = fa.flatMap(f)
      def pure[A](a: A): Codensity[F, A] = Codensity.pure(a)
    }

  implicit def codensityFreeLike[F[_], M[_]](implicit MF: FreeLike[F, M], F: Functor[F]): FreeLike[F, Codensity[M, ?]] =
    new FreeLike[F, Codensity[M, ?]] {
      def wrap[A](fca: F[Codensity[M, A]]): Codensity[M, A] = 
        new Codensity[M, A] {
          def apply[B](f: A => M[B]): M[B] = MF.wrap(F.map(fca){ ca => ca.apply(f) })
        }
    }
}
