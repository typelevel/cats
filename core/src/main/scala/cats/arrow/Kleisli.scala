package cats
package arrow

/**
 * Represents a function `A => M[B]`.
 */
final case class Kleisli[M[_], A, B](run: A => M[B]) { self =>
  import Kleisli._

  def apply(a: A): M[B] = run(a)

  def dimap[C, D](f: C => A, g: B => D)(implicit b: Functor[M]): Kleisli[M, C, D] =
    Kleisli(c => b.map(run(f(c)))(g))

  def andThen[C](f: B => M[C])(implicit b: FlatMap[M]): Kleisli[M, A, C] =
    Kleisli((a: A) => b.flatMap(run(a))(f))
  
  def andThen[C](k: Kleisli[M, B, C])(implicit b: FlatMap[M]): Kleisli[M, A, C] =
    this andThen k.run
  
  def compose[Z](f: Z => M[A])(implicit M: FlatMap[M]): Kleisli[M, Z, B] =
    Kleisli((z: Z) => M.flatMap(f(z))(run))
  
  def compose[Z](k: Kleisli[M, Z, A])(implicit b: FlatMap[M]): Kleisli[M, Z, B] =
    this compose k.run

  def map[C](f: B => C)(implicit M: Functor[M]): Kleisli[M, A, C] =
    Kleisli(a => M.map(run(a))(f))

  def mapK[N[_], C](f: M[B] => N[C]): Kleisli[N, A, C] =
    Kleisli(run andThen f)

  def flatMap[C](f: B => M[C])(implicit M: FlatMap[M]): Kleisli[M, A, C] =
    Kleisli(a => M.flatMap(run(a))(f))

  def flatMapK[C](f: B => Kleisli[M, A, C])(implicit M: FlatMap[M]): Kleisli[M, A, C] =
    Kleisli((r: A) => M.flatMap[B, C](run(r))(((b: B) => f(b).run(r))))

  def traverse[F[_]](f: F[A])(implicit M: Applicative[M], F: Traverse[F]): M[F[B]] =
    F.traverse(f)(run)

  def lift[F[_]: Applicative]: Kleisli[λ[α => F[M[α]]], A, B] =
    Kleisli[λ[α => F[M[α]]], A, B](a => Applicative[F].pure(run(a)))

  def lower(implicit M: Monad[M]): Kleisli[M, A, M[B]] =
    Kleisli(a => M.pure(run(a)))
}

object Kleisli {
  def ask[M[_]: Applicative, A]: Kleisli[M, A, A] =
    Kleisli(Applicative[M].pure)

  def local[M[_], A, R](f: R => R)(fa: Kleisli[M, R, A]): Kleisli[M, R, A] =
    Kleisli(f andThen fa.run)
}
