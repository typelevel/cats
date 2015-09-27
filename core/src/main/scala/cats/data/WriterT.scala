package cats
package data

final case class WriterT[F[_], L, V](run: F[(L, V)]) {
  def written(implicit functorF: Functor[F]): F[L] =
    functorF.map(run)(_._1)

  def value(implicit functorF: Functor[F]): F[V] =
    functorF.map(run)(_._2)

  def map[Z](fn: V => Z)(implicit functorF: Functor[F]): WriterT[F, L, Z] =
    WriterT {
      functorF.map(run) { z => (z._1, fn(z._2)) }
    }

  def flatMap[U](f: V => WriterT[F, L, U])(implicit flatMapF: FlatMap[F], semigroupL: Semigroup[L]): WriterT[F, L, U] =
    WriterT {
      flatMapF.flatMap(run) { lv =>
        flatMapF.map(f(lv._2).run) { lv2 =>
          (semigroupL.combine(lv._1, lv2._1), lv2._2)
        }
      }
    }

  def mapBoth[M, U](f: (L, V) => (M, U))(implicit functorF: Functor[F]): WriterT[F, M, U] =
    WriterT { functorF.map(run)(f.tupled) }

  def mapWritten[M](f: L => M)(implicit functorF: Functor[F]): WriterT[F, M, V] =
    mapBoth((l, v) => (f(l), v))

  def swap(implicit functorF: Functor[F]): WriterT[F, V, L] =
    mapBoth((l, v) => (v, l))

  def reset(implicit monoidL: Monoid[L], functorF: Functor[F]): WriterT[F, L, V] =
    mapWritten(_ => monoidL.empty)
}
object WriterT extends WriterTInstances with WriterTFunctions

sealed abstract class WriterTInstances {
  implicit def writerTMonad[F[_], L](implicit monadF: Monad[F], monoidL: Monoid[L]) = {
    new Monad[WriterT[F, L, ?]] {
      override def pure[A](a: A): WriterT[F, L, A] =
        WriterT.value[F, L, A](a)

      override def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] =
        fa.flatMap(a => f(a))
    }
  }

  implicit def writerTEq[F[_], L, V](implicit F: Eq[F[(L, V)]]): Eq[WriterT[F, L, V]] =
    F.on(_.run)
}

trait WriterTFunctions {
  def putT[F[_], L, V](vf: F[V])(l: L)(implicit functorF: Functor[F]): WriterT[F, L, V] =
    WriterT(functorF.map(vf)(v => (l, v)))

  def put[F[_], L, V](v: V)(l: L)(implicit applicativeF: Applicative[F]): WriterT[F, L, V] =
    WriterT.putT[F, L, V](applicativeF.pure(v))(l)

  def tell[F[_], L](l: L)(implicit applicativeF: Applicative[F]): WriterT[F, L, Unit] =
    WriterT.put[F, L, Unit](())(l)

  def value[F[_], L, V](v: V)(implicit applicativeF: Applicative[F], monoidL: Monoid[L]): WriterT[F, L, V] =
    WriterT.put[F, L, V](v)(monoidL.empty)

  def valueT[F[_], L, V](vf: F[V])(implicit functorF: Functor[F], monoidL: Monoid[L]): WriterT[F, L, V] =
    WriterT.putT[F, L, V](vf)(monoidL.empty)
}


