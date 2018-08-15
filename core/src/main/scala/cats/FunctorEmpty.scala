package cats

import simulacrum.typeclass

/**
 * `FunctorEmpty[F]` allows you to `map` and filter out elements simultaneously.
 */
@typeclass
trait FunctorEmpty[F[_]] extends Serializable {
  def functor: Functor[F]

  def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B]

  def collect[A, B](fa: F[A])(f: PartialFunction[A, B]): F[B] =
    mapFilter(fa)(f.lift)

  def flattenOption[A](fa: F[Option[A]]): F[A] =
    mapFilter(fa)(identity)

  def filter[A](fa: F[A])(f: A => Boolean): F[A] =
    mapFilter(fa)(a => if (f(a)) Some(a) else None)
}

object FunctorEmpty {
  implicit def catsFunctorForFunctorEmpty[F[_]](fe: FunctorEmpty[F]): Functor[F] =
    fe.functor
}

