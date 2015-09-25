package cats
package data

final case class WriterT[T[_], L, V] (run: T[(L, V)]) {
  def written (implicit functorT: Functor[T]): T[L] =
    functorT.map (run)(_._1)

  def value (implicit functorT: Functor[T]): T[V] =
    functorT.map (run)(_._2)

  def map[Z](fn: V => Z)(implicit functorT: Functor[T]): WriterT[T, L, Z] =
    WriterT {
      functorT.map (run) { z =>
        (z._1, fn (z._2))
      }
    }

  def flatMap[U](f: V => WriterT[T, L, U])(implicit monadT: Monad[T], semigroupL: Semigroup[L]): WriterT[T, L, U] =
    WriterT {
      monadT.flatMap (run) { lv =>
        monadT.map (f (lv._2).run) { lv2 =>
          (semigroupL.combine (lv._1, lv2._1), lv2._2)
        }
      }
    }

  def mapValue[M, U] (f: ((L, V)) => (M, U))(implicit functorT: Functor[T]): WriterT[T, M, U] =
    WriterT { functorT.map (run)(f) }

  def mapWritten[M] (f: L => M)(implicit functorT: Functor[T]): WriterT[T, M, V] =
    mapValue (wa => (f (wa._1), wa._2))

  def swap (implicit functorT: Functor[T]): WriterT[T, V, L] =
    mapValue (wa => (wa._2, wa._1))

  def reset (implicit monoidL: Monoid[L], functorT: Functor[T]): WriterT[T, L, V] =
    mapWritten (_ => monoidL.empty)
}
object WriterT extends WriterTInstances with WriterTFunctions

sealed abstract class WriterTInstances {
  implicit def writerTMonad[T[_], L] (implicit monadT: Monad[T], monoidL: Monoid[L]) = {
    new Monad[({type WT[$] = WriterT[T, L, $]})#WT] {
      override def pure[A] (a: A): ({type WT[$] = WriterT[T, L, $]})#WT[A] =
        WriterT.value[T, L, A] (a)

      override def flatMap[A, B] (fa: ({type WT[$] = WriterT[T, L, $]})#WT[A])(f: A => ({type WT[$] = WriterT[T, L, $]})#WT[B]): WriterT[T, L, B] =
        fa.flatMap (a => f (a))

      override def ap[A, B] (fa: ({type WT[$] = WriterT[T, L, $]})#WT[A])(ff: ({type WT[$] = WriterT[T, L, $]})#WT[A => B]): ({type WT[$] = WriterT[T, L, $]})#WT[B] =
        fa.flatMap (a => ff.map (f => f (a)))
    }
  }
}

trait WriterTFunctions {
  def putT[T[_], L, V] (vt: T[V])(l: L)(implicit functorT: Functor[T]): WriterT[T, L, V] =
    WriterT (functorT.map (vt)(v => (l, v)))

  def put[T[_], L, V] (v: V)(l: L)(implicit functorT: Functor[T], applicativeT: Applicative[T]): WriterT[T, L, V] =
    WriterT.putT[T, L, V](applicativeT.pure (v))(l)

  def tell[T[_], L] (l: L)(implicit functorT: Functor[T], applicativeT: Applicative[T]): WriterT[T, L, Unit] =
    WriterT.put[T, L, Unit](())(l)

  def value[T[_], L, V] (v: V)(implicit functorT: Functor[T], applicativeT: Applicative[T], monoidL: Monoid[L]): WriterT[T, L, V] =
    WriterT.put[T, L, V](v)(monoidL.empty)

  def valueT[T[_], L, V] (vt: T[V])(implicit functorT: Functor[T], monoidL: Monoid[L]): WriterT[T, L, V] =
    WriterT.putT[T, L, V](vt)(monoidL.empty)
}


