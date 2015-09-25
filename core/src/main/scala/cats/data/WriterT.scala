package cats
package data

final case class WriterT[F[_], L, V](run: F[(L, V)]) {
  def written(implicit functorF: Functor[F]): F[L] =
    functorF.map(run)(_._1)

  def value(implicit functorF: Functor[F]): F[V] =
    functorF.map(run)(_._2)

  def map[Z](fn: V => Z)(implicit functorF: Functor[F]): WriterT[F, L, Z] =
    WriterT {
      functorF.map(run) { z => (z._1, fn(z._2))
      }
    }

  def flatMap[U](f: V => WriterT[F, L, U])(implicit flatMapF: FlatMap[F], semigroupL: Semigroup[L]): WriterT[F, L, U] =
    WriterT {
      flatMapF.flatMap(run) { lv =>
        flatMapF.map(f(lv._2).run) { lv2 => (semigroupL.combine(lv._1, lv2._1), lv2._2)
        }
      }
    }

  def mapBoth[M, U](f: ((L, V)) => (M, U))(implicit functorF: Functor[F]): WriterT[F, M, U] =
    WriterT { functorF.map(run)(f) }

  def mapWritten[M](f: L => M)(implicit functorF: Functor[F]): WriterT[F, M, V] =
    mapBoth(wa =>(f(wa._1), wa._2))

  def swap(implicit functorF: Functor[F]): WriterT[F, V, L] =
    mapBoth(wa =>(wa._2, wa._1))

  def reset(implicit monoidL: Monoid[L], functorF: Functor[F]): WriterT[F, L, V] =
    mapWritten(_ => monoidL.empty)
}
object WriterT extends WriterTInstances with WriterTFunctions

sealed abstract class WriterTInstances {
  implicit def writerTMonad[F[_], L](implicit monadF: Monad[F], monoidL: Monoid[L]) = {
    new Monad[({type WT[$] = WriterT[F, L, $]})#WT] {
      override def pure[A](a: A):({type WT[$] = WriterT[F, L, $]})#WT[A] =
        WriterT.value[F, L, A](a)

      override def flatMap[A, B](fa:({type WT[$] = WriterT[F, L, $]})#WT[A])(f: A =>({type WT[$] = WriterT[F, L, $]})#WT[B]): WriterT[F, L, B] =
        fa.flatMap(a => f(a))

      override def ap[A, B](fa:({type WT[$] = WriterT[F, L, $]})#WT[A])(ff:({type WT[$] = WriterT[F, L, $]})#WT[A => B]):({type WT[$] = WriterT[F, L, $]})#WT[B] =
        fa.flatMap(a => ff.map(f => f(a)))
    }
  }
}

trait WriterTFunctions {
  def putT[F[_], L, V](vf: F[V])(l: L)(implicit functorF: Functor[F]): WriterT[F, L, V] =
    WriterT(functorF.map(vf)(v =>(l, v)))

  def put[F[_], L, V](v: V)(l: L)(implicit applicativeF: Applicative[F]): WriterT[F, L, V] =
    WriterT.putT[F, L, V](applicativeF.pure(v))(l)

  def tell[F[_], L](l: L)(implicit applicativeF: Applicative[F]): WriterT[F, L, Unit] =
    WriterT.put[F, L, Unit](())(l)

  def value[F[_], L, V](v: V)(implicit applicativeF: Applicative[F], monoidL: Monoid[L]): WriterT[F, L, V] =
    WriterT.put[F, L, V](v)(monoidL.empty)

  def valueT[F[_], L, V](vf: F[V])(implicit functorF: Functor[F], monoidL: Monoid[L]): WriterT[F, L, V] =
    WriterT.putT[F, L, V](vf)(monoidL.empty)
}


