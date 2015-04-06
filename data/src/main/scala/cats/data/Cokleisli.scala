package cats.data

import cats.functor.{Profunctor, Strong}
import cats.{CoflatMap, Comonad, Functor, Monad}

final case class Cokleisli[F[_], A, B](run: F[A] => B) { self =>

  def dimap[C, D](f: C => A)(g: B => D)(implicit F: Functor[F]): Cokleisli[F, C, D] =
    Cokleisli(fc => g(run(F.map(fc)(f))))

  def lmap[C](f: C => A)(implicit F: Functor[F]): Cokleisli[F, C, B] =
    Cokleisli(fc => run(F.map(fc)(f)))

  def map[C](f: B => C): Cokleisli[F, A, C] =
    Cokleisli(f compose run)

  def contramapValue[C](f: F[C] => F[A]): Cokleisli[F, C,  B] =
    Cokleisli(run compose f)

  def flatMap[C](f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
    Cokleisli(fa => f(self.run(fa)).run(fa))

  def compose[C](c: Cokleisli[F, C, A])(implicit F: CoflatMap[F]): Cokleisli[F, C, B] =
    Cokleisli(fc => run(F.coflatMap(fc)(c.run)))

  def andThen[C](c: Cokleisli[F, B, C])(implicit F: CoflatMap[F]): Cokleisli[F, A, C] =
    c compose this

  def first[C](implicit F: Comonad[F]): Cokleisli[F, (A, C), (B, C)] =
    Cokleisli(fac => run(F.map(fac)(_._1)) -> F.extract(F.map(fac)(_._2)))

  def second[C](implicit F: Comonad[F]): Cokleisli[F, (C, A), (C, B)] =
    Cokleisli(fca => F.extract(F.map(fca)(_._1)) -> run(F.map(fca)(_._2)))
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

sealed abstract class CokleisliInstances extends CokleisliInstances0 {
  implicit def cokleisliStrong[F[_]](implicit ev: Comonad[F]): Strong[Cokleisli[F, ?, ?]] =
    new CokleisliStrong[F] { def F: Comonad[F] = ev }

  implicit def cokleisliMonad[F[_], A]: Monad[Cokleisli[F, A, ?]] = new Monad[Cokleisli[F, A, ?]] {
    def pure[B](x: B): Cokleisli[F, A, B] =
      Cokleisli.pure(x)

    def flatMap[B, C](fa: Cokleisli[F, A, B])(f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
      fa.flatMap(f)

    override def map[B, C](fa: Cokleisli[F, A, B])(f: B => C): Cokleisli[F, A, C] =
      fa.map(f)
  }
}

sealed abstract class CokleisliInstances0 {
  implicit def cokleisliProfunctor[F[_]](implicit ev: Functor[F]): Profunctor[Cokleisli[F, ?, ?]] =
    new CokleisliProfunctor[F] { def F: Functor[F] = ev }
}

private trait CokleisliStrong[F[_]] extends Strong[Cokleisli[F, ?, ?]] with CokleisliProfunctor[F] {
  implicit def F: Comonad[F]

  def first[A, B, C](fa: Cokleisli[F, A, B]): Cokleisli[F, (A, C), (B, C)] =
    fa.first[C]

  def second[A, B, C](fa: Cokleisli[F, A, B]): Cokleisli[F, (C, A), (C, B)] =
    fa.second[C]
}

private trait CokleisliProfunctor[F[_]] extends Profunctor[Cokleisli[F, ?, ?]] {
  implicit def F: Functor[F]

  def dimap[A, B, C, D](fab: Cokleisli[F, A, B])(f: C => A)(g: B => D): Cokleisli[F, C, D] =
    fab.dimap(f)(g)

  override def lmap[A, B, C](fab: Cokleisli[F, A, B])(f: C => A): Cokleisli[F, C, B] =
    fab.lmap(f)

  override def rmap[A, B, C](fab: Cokleisli[F, A, B])(f: B => C): Cokleisli[F, A, C] =
    fab.map(f)
}
