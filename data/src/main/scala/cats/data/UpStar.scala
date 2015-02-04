package cats.data

import cats._
import cats.functor.{Strong, Profunctor}


final case class UpStar[F[_], A, B](fab: A => F[B]){
  def dimap[C, D](f: C => A)(g: B => D)(implicit F: Functor[F]): UpStar[F, C, D] =
    UpStar(c => Functor[F].map(fab(f(c)))(g))

  def lmap[C](f: C => A): UpStar[F, C, B] =
    UpStar(fab compose f)

  def rmap[C](f: B => C)(implicit F: Functor[F]): UpStar[F, A, C] =
    UpStar(a => Functor[F].map(fab(a))(f))

  def first[C](implicit F: Functor[F]): UpStar[F, (A, C), (B, C)] =
    UpStar{ case (a, c) => F.fproduct(fab(a))(_ => c)}

  def second[C](implicit F: Functor[F]): UpStar[F, (C, A), (C, B)] =
    UpStar{ case (c, a) => F.map(fab(a))(c -> _)}

  def flatMap[C](f: B => UpStar[F, A, C])(implicit F: FlatMap[F]): UpStar[F, A, C] =
    UpStar(a => F.flatMap(fab(a))(b => f(b).fab(a)))

  def apply[C](f: UpStar[F, A, B => C])(implicit F: Apply[F]): UpStar[F, A, C] =
    UpStar(a => F.apply(fab(a))(f.fab(a)))
}

object UpStar extends UpStarInstances {
  def pure[F[_], A, B](x: B)(implicit F: Applicative[F]): UpStar[F, A, B] =
    UpStar(_ => F.pure(x))
}

sealed abstract class UpStarInstances extends UpStarInstances0 {
  implicit def upStarStrong[F[_]: Functor]: Strong[UpStar[F, ?, ?]] = new Strong[UpStar[F, ?, ?]]{
    override def lmap[A, B, C](fab: UpStar[F, A, B])(f: C => A): UpStar[F, C, B] =
      fab.lmap(f)

    override def rmap[A, B, C](fab: UpStar[F, A, B])(f: B => C): UpStar[F, A, C] =
      fab.rmap(f)

    override def dimap[A, B, C, D](fab: UpStar[F, A, B])(f: C => A)(g: B => D): UpStar[F, C, D] =
      fab.dimap(f)(g)

    def first[A, B, C](fa: UpStar[F, A, B]): UpStar[F, (A, C), (B, C)] =
      fa.first[C]

    def second[A, B, C](fa: UpStar[F, A, B]): UpStar[F, (C, A), (C, B)] =
      fa.second[C]
  }

  implicit def upStarMonad[F[_]: Monad, A]: Monad[UpStar[F, A, ?]] = new Monad[UpStar[F, A, ?]] {
    def pure[B](x: B): UpStar[F, A, B] =
      UpStar.pure[F, A, B](x)

    def flatMap[B, C](fa: UpStar[F, A, B])(f: B => UpStar[F, A, C]): UpStar[F, A, C] =
      fa.flatMap(f)
  }
}

sealed abstract class UpStarInstances0 extends UpStarInstances1 {
  implicit def upStarFlatMap[F[_]: FlatMap, A]: FlatMap[UpStar[F, A, ?]] = new FlatMap[UpStar[F, A, ?]] {
    def flatMap[B, C](fa: UpStar[F, A, B])(f: B => UpStar[F, A, C]): UpStar[F, A, C] =
      fa.flatMap(f)

    def map[B, C](fa: UpStar[F, A, B])(f: B => C): UpStar[F, A, C] =
      fa.rmap(f)
  }
}

sealed abstract class UpStarInstances1 extends UpStarInstances2 {
  implicit def upStarApplicative[F[_]: Applicative, A]: Applicative[UpStar[F, A, ?]] = new Applicative[UpStar[F, A, ?]] {
    def pure[B](x: B): UpStar[F, A, B] =
      UpStar.pure[F, A, B](x)

    def apply[B, C](fa: UpStar[F, A, B])(f: UpStar[F, A, B => C]): UpStar[F, A, C] =
      fa.apply(f)
  }
}

sealed abstract class UpStarInstances2 extends UpStarInstances3 {
  implicit def upStarApply[F[_]: Apply, A]: Apply[UpStar[F, A, ?]] = new Apply[UpStar[F, A, ?]] {
    def apply[B, C](fa: UpStar[F, A, B])(f: UpStar[F, A, B => C]): UpStar[F, A, C] =
      fa.apply(f)

    def map[B, C](fa: UpStar[F, A, B])(f: B => C): UpStar[F, A, C] =
      fa.rmap(f)
  }
}

sealed abstract class UpStarInstances3 {
  implicit def upStarFunctor[F[_]: Functor, A]: Functor[UpStar[F, A, ?]] = new Functor[UpStar[F, A, ?]] {
    def map[B, C](fa: UpStar[F, A, B])(f: B => C): UpStar[F, A, C] =
      fa.rmap(f)
  }
}