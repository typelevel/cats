package cats.data

import cats.functor.Profunctor
import cats.{Monad, CoFlatMap, Functor}

final case class Cokleisli[F[_], A, B](run: F[A] => B) { self =>

  def dimap[C, D](f: C => A)(g: B => D)(implicit b: Functor[F]): Cokleisli[F, C, D] =
    Cokleisli(fc => g(run(b.map(fc)(f))))

  def lmap[C](f: C => A)(implicit F: Functor[F]): Cokleisli[F, C, B] =
    Cokleisli(fc => run(F.map(fc)(f)))

  def map[C](f: B => C): Cokleisli[F, A, C] =
    Cokleisli(f compose run)

  def contramapValue[C](f: F[C] => F[A]): Cokleisli[F, C,  B] =
    Cokleisli(run compose f)

  def flatMap[C](f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
    Cokleisli(fa => f(self.run(fa)).run(fa))

  def compose[C](c: Cokleisli[F, C, A])(implicit F: CoFlatMap[F]): Cokleisli[F, C, B] =
    Cokleisli(fc => run(F.coflatMap(fc)(c.run)))

  def andThen[C](c: Cokleisli[F, B, C])(implicit F: CoFlatMap[F]): Cokleisli[F, A, C] =
    c compose this
}

object Cokleisli extends CokleisliInstances with CokleisliFunctions {
  def pure[F[_], A, B](x: B): Cokleisli[F, A, B] =
    Cokleisli(_ => x)
}

sealed trait CokleisliFunctions {
  /** creates a [[Cokleisli]] from a function */
  def cokleisli[F[_], A, B](f: F[A] => B): Cokleisli[F, A, B] =
    Cokleisli(f)
}

sealed abstract class CokleisliInstances {
  implicit def cokleisliProfunctor[F[_]: Functor]: Profunctor[Cokleisli[F, ?, ?]] = new Profunctor[Cokleisli[F, ?, ?]] {
    def dimap[A, B, C, D](fab: Cokleisli[F, A, B])(f: C => A)(g: B => D): Cokleisli[F, C, D] =
      fab.dimap(f)(g)
    override def lmap[A, B, C](fab: Cokleisli[F, A, B])(f: C => A): Cokleisli[F, C, B] =
      fab.lmap(f)

    override def rmap[A, B, C](fab: Cokleisli[F, A, B])(f: B => C): Cokleisli[F, A, C] =
      fab.map(f)
  }

  implicit def cokleisliMonad[F[_], A]: Monad[Cokleisli[F, A, ?]] = new Monad[Cokleisli[F, A, ?]] {
    def pure[B](x: B): Cokleisli[F, A, B] =
      Cokleisli.pure(x)

    def flatMap[B, C](fa: Cokleisli[F, A, B])(f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
      fa.flatMap(f)

    override def map[B, C](fa: Cokleisli[F, A, B])(f: B => C): Cokleisli[F, A, C] =
      fa.map(f)
  }
}
