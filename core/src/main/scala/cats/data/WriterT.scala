package cats
package data

import cats.kernel.instances.tuple._
import cats.functor.{Bifunctor, Contravariant}
import cats.syntax.semigroup._

final case class WriterT[F[_], L, V](run: F[(L, V)]) {
  def write(l: L)(implicit functorF: Functor[F], semigroupL: Semigroup[L]): WriterT[F, L, V] =
    mapWritten(_ |+| l)

  def written(implicit functorF: Functor[F]): F[L] =
    functorF.map(run)(_._1)

  def value(implicit functorF: Functor[F]): F[V] =
    functorF.map(run)(_._2)

  def ap[Z](f: WriterT[F, L, V => Z])(implicit F: Apply[F], L: Semigroup[L]): WriterT[F, L, Z] =
    WriterT(
      F.map2(f.run, run){
        case ((l1, fvz), (l2, v)) => (L.combine(l1, l2), fvz(v))
      })

  def map[Z](fn: V => Z)(implicit functorF: Functor[F]): WriterT[F, L, Z] =
    WriterT {
      functorF.map(run) { z => (z._1, fn(z._2)) }
    }

  def contramap[Z](fn: Z => V)(implicit F: Contravariant[F]): WriterT[F, L, Z] =
    WriterT {
      F.contramap(run) { z => (z._1, fn(z._2)) }
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

  def bimap[M, U](f: L => M, g: V => U)(implicit functorF: Functor[F]): WriterT[F, M, U] =
    mapBoth((l, v) => (f(l), g(v)))

  def mapWritten[M](f: L => M)(implicit functorF: Functor[F]): WriterT[F, M, V] =
    mapBoth((l, v) => (f(l), v))

  def swap(implicit functorF: Functor[F]): WriterT[F, V, L] =
    mapBoth((l, v) => (v, l))

  def reset(implicit monoidL: Monoid[L], functorF: Functor[F]): WriterT[F, L, V] =
    mapWritten(_ => monoidL.empty)

  def show(implicit F: Show[F[(L, V)]]): String = F.show(run)
}
object WriterT extends WriterTInstances with WriterTFunctions

private[data] sealed abstract class WriterTInstances extends WriterTInstances0 {

  implicit def catsDataMonadForWriterTId[L:Monoid]: Monad[WriterT[Id, L, ?]] =
    catsDataMonadWriterForWriterT[Id, L]

  implicit def catsDataEqForWriterTId[L: Eq, V: Eq]: Eq[WriterT[Id, L, V]] =
    catsDataEqForWriterT[Id, L, V]

  implicit def catsDataBifunctorForWriterT[F[_]:Functor]: Bifunctor[WriterT[F, ?, ?]] =
    new Bifunctor[WriterT[F, ?, ?]] {
      def bimap[A, B, C, D](fab: WriterT[F, A, B])(f: A => C, g: B => D): WriterT[F, C, D] =
        fab.bimap(f, g)
    }

  implicit def catsDataTransLiftForWriterT[W](implicit W: Monoid[W]): TransLift.Aux[WriterT[?[_], W, ?], Functor] =
    new TransLift[WriterT[?[_], W, ?]] {
      type TC[M[_]] = Functor[M]

      def liftT[M[_]: Functor, A](ma: M[A]): WriterT[M, W, A] =
        WriterT(Functor[M].map(ma)((W.empty, _)))
    }

  implicit def catsDataShowForWriterT[F[_], L, V](implicit F: Show[F[(L, V)]]): Show[WriterT[F, L, V]] = new Show[WriterT[F, L, V]] {
    override def show(f: WriterT[F, L, V]): String = f.show
  }

  implicit def catsDataMonoidForWriterTId[L:Monoid, V:Monoid]: Monoid[WriterT[Id, L, V]] =
    catsDataMonoidForWriterT[Id, L, V]
}

private[data] sealed abstract class WriterTInstances0 extends WriterTInstances1 {
  implicit def catsDataMonadCombineForWriterT[F[_], L](implicit F: MonadCombine[F], L: Monoid[L]): MonadCombine[WriterT[F, L, ?]] =
    new WriterTMonadCombine[F, L] {
      implicit val F0: MonadCombine[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataFlatMapForWriterTId[L:Semigroup]: FlatMap[WriterT[Id, L, ?]] =
    catsDataFlatMapForWriterT[Id, L]

  implicit def catsDataEqForWriterT[F[_], L, V](implicit F: Eq[F[(L, V)]]): Eq[WriterT[F, L, V]] =
    F.on(_.run)

  implicit def catsDataSemigroupForWriterTId[L:Semigroup, V:Semigroup]: Semigroup[WriterT[Id, L, V]] =
    catsDataSemigroupForWriterT[Id, L, V]
}

private[data] sealed abstract class WriterTInstances1 extends WriterTInstances2 {
  implicit def catsDataMonadFilterForWriterT[F[_], L](implicit F: MonadFilter[F], L: Monoid[L]): MonadFilter[WriterT[F, L, ?]] =
    new WriterTMonadFilter[F, L] {
      implicit val F0: MonadFilter[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataMonoidForWriterT[F[_], L, V](implicit W: Monoid[F[(L, V)]]): Monoid[WriterT[F, L, V]] =
    new WriterTMonoid[F, L, V] {
      implicit val F0: Monoid[F[(L, V)]] = W
    }

  implicit def catsDataCoflatMapForWriterTId[L]: CoflatMap[WriterT[Id, L, ?]] =
    catsDataCoflatMapForWriterT[Id, L]

}

private[data] sealed abstract class WriterTInstances2 extends WriterTInstances3 {
  implicit def catsDataMonadWriterForWriterT[F[_], L](implicit F: Monad[F], L: Monoid[L]): MonadWriter[WriterT[F, L, ?], L] =
    new WriterTMonadWriter[F, L] {
      implicit val F0: Monad[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataSemigroupForWriterT[F[_], L, V](implicit W: Semigroup[F[(L, V)]]): Semigroup[WriterT[F, L, V]] =
    new WriterTSemigroup[F, L, V] {
      implicit val F0: Semigroup[F[(L, V)]] = W
    }
}

private[data] sealed abstract class WriterTInstances3 extends WriterTInstances4 {
  implicit def catsDataAlternativeForWriterT[F[_], L](implicit F: Alternative[F], L: Monoid[L]): Alternative[WriterT[F, L, ?]] =
    new WriterTAlternative[F, L] {
      implicit val F0: Alternative[F] = F
      implicit val L0: Monoid[L] = L
    }

}

private[data] sealed abstract class WriterTInstances4 extends WriterTInstances5 {
  implicit def catsDataApplicativeForWriterT[F[_], L](implicit F: Applicative[F], L: Monoid[L]): Applicative[WriterT[F, L, ?]] =
    new WriterTApplicative[F, L] {
      implicit val F0: Applicative[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataMonoidKForWriterT[F[_], L](implicit F: MonoidK[F]): MonoidK[WriterT[F, L, ?]] =
    new WriterTMonoidK[F, L] {
      implicit val F0: MonoidK[F] = F
    }
}

private[data] sealed abstract class WriterTInstances5 extends WriterTInstances6 {
  implicit def catsDataFlatMapForWriterT[F[_], L](implicit F: FlatMap[F], L: Semigroup[L]): FlatMap[WriterT[F, L, ?]] =
    new WriterTFlatMap[F, L] {
      implicit val F0: FlatMap[F] = F
      implicit val L0: Semigroup[L] = L
    }

  implicit def catsDataSemigroupKForWriterT[F[_], L](implicit F: SemigroupK[F]): SemigroupK[WriterT[F, L, ?]] =
    new WriterTSemigroupK[F, L] {
      implicit val F0: SemigroupK[F] = F
    }
}

private[data] sealed abstract class WriterTInstances6 extends WriterTInstances7 {
  implicit def catsDataApplyForWriterT[F[_], L](implicit F: Apply[F], L: Semigroup[L]): Apply[WriterT[F, L, ?]] =
    new WriterTApply[F, L] {
      implicit val F0: Apply[F] = F
      implicit val L0: Semigroup[L] = L
    }
}

private[data] sealed abstract class WriterTInstances7 {

  implicit def catsDataCoflatMapForWriterT[F[_], L](implicit F: Functor[F]): CoflatMap[WriterT[F, L, ?]] =
    new WriterTCoflatMap[F, L] {
      implicit val F0: Functor[F] = F
    }

  implicit def catsDataContravariantForWriterT[F[_], L](implicit F: Contravariant[F]): Contravariant[WriterT[F, L, ?]] = new WriterTContravariant[F, L] {
    implicit val F0: Contravariant[F] = F
  }
}

private[data] sealed trait WriterTFunctor[F[_], L] extends Functor[WriterT[F, L, ?]] {
  implicit def F0: Functor[F]

  def map[A, B](fa: WriterT[F, L, A])(f: A => B): WriterT[F, L, B] =
    fa.map(f)
}

private[data] sealed trait WriterTContravariant[F[_], L] extends Contravariant[WriterT[F, L, ?]] {
  implicit def F0: Contravariant[F]

  def contramap[A, B](fa: WriterT[F, L, A])(f: B => A): WriterT[F, L, B] =
    fa.contramap(f)
}

private[data] sealed trait WriterTApply[F[_], L] extends WriterTFunctor[F, L] with Apply[WriterT[F, L, ?]] {
  override implicit def F0: Apply[F]
  implicit def L0: Semigroup[L]

  def ap[A, B](f: WriterT[F, L, A => B])(fa: WriterT[F, L, A]): WriterT[F, L, B] =
    fa ap f
  override def product[A, B](fa: WriterT[F, L, A], fb: WriterT[F, L, B]): WriterT[F, L, (A, B)] =
    WriterT(F0.map(F0.product(fa.run, fb.run)) { case ((l1, a), (l2, b)) => (L0.combine(l1, l2), (a, b)) })
}

private[data] sealed trait WriterTFlatMap[F[_], L] extends WriterTApply[F, L] with FlatMap[WriterT[F, L, ?]] {
  override implicit def F0: FlatMap[F]
  implicit def L0: Semigroup[L]

  def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] =
    fa flatMap f
}

private[data] sealed trait WriterTApplicative[F[_], L] extends WriterTApply[F, L] with Applicative[WriterT[F, L, ?]] {
  override implicit def F0: Applicative[F]
  override implicit def L0: Monoid[L]

  def pure[A](a: A): WriterT[F, L, A] =
    WriterT.value[F, L, A](a)
}

private[data] sealed trait WriterTMonad[F[_], L] extends WriterTApplicative[F, L] with Monad[WriterT[F, L, ?]] {
  override implicit def F0: Monad[F]
  override implicit def L0: Monoid[L]

  def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] =
    fa.flatMap(f)
}

private[data] sealed trait WriterTMonadWriter[F[_], L] extends MonadWriter[WriterT[F, L, ?], L] with WriterTMonad[F, L] {
  def writer[A](aw: (L, A)): WriterT[F, L, A] =
    WriterT.put(aw._2)(aw._1)

  def listen[A](fa: WriterT[F, L, A]): WriterT[F, L, (L, A)] =
    WriterT(F0.flatMap(fa.value)(a => F0.map(fa.written)(l => (l, (l, a)))))

  def pass[A](fa: WriterT[F, L, (L => L, A)]): WriterT[F, L, A] =
    WriterT(F0.flatMap(fa.value) { case (f, a) => F0.map(fa.written)(l => (f(l), a)) })

  override def write(l: L): WriterT[F, L, Unit] = WriterT.write(l)
}

private[data] sealed trait WriterTSemigroupK[F[_], L] extends SemigroupK[WriterT[F, L, ?]] {
  implicit def F0: SemigroupK[F]

  def combineK[A](x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.combineK(x.run, y.run))
}

private[data] sealed trait WriterTMonoidK[F[_], L] extends MonoidK[WriterT[F, L, ?]] with WriterTSemigroupK[F, L] {
  override implicit def F0: MonoidK[F]

  def empty[A]: WriterT[F, L, A] = WriterT(F0.empty)
}

private[data] sealed trait WriterTAlternative[F[_], L] extends Alternative[WriterT[F, L, ?]] with WriterTMonoidK[F, L] with WriterTApplicative[F, L] {
  override implicit def F0: Alternative[F]
}

private[data] sealed trait WriterTMonadFilter[F[_], L] extends MonadFilter[WriterT[F, L, ?]] with WriterTMonad[F, L] {
  override implicit def F0: MonadFilter[F]

  def empty[A]: WriterT[F, L, A] = WriterT(F0.empty)
}

private[data] sealed trait WriterTMonadCombine[F[_], L] extends MonadCombine[WriterT[F, L, ?]] with WriterTMonad[F, L] with WriterTAlternative[F, L] {
  override implicit def F0: MonadCombine[F]
}

private[data] sealed trait WriterTSemigroup[F[_], L, A] extends Semigroup[WriterT[F, L, A]] {
  implicit def F0: Semigroup[F[(L, A)]]

  def combine(x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.combine(x.run, y.run))
}

private[data] sealed trait WriterTMonoid[F[_], L, A] extends Monoid[WriterT[F, L, A]] with WriterTSemigroup[F, L, A] {
  override implicit def F0: Monoid[F[(L, A)]]

  def empty: WriterT[F, L, A] = WriterT(F0.empty)
}

private[data] sealed trait WriterTCoflatMap[F[_], L] extends CoflatMap[WriterT[F, L, ?]] with WriterTFunctor[F, L] {

  def coflatMap[A, B](fa: WriterT[F, L, A])(f: WriterT[F, L, A] => B): WriterT[F, L, B] = fa.map(_ => f(fa))
}


trait WriterTFunctions {
  def putT[F[_], L, V](vf: F[V])(l: L)(implicit functorF: Functor[F]): WriterT[F, L, V] =
    WriterT(functorF.map(vf)(v => (l, v)))

  def put[F[_], L, V](v: V)(l: L)(implicit applicativeF: Applicative[F]): WriterT[F, L, V] =
    WriterT.putT[F, L, V](applicativeF.pure(v))(l)

  def write[F[_], L](l: L)(implicit applicativeF: Applicative[F]): WriterT[F, L, Unit] =
    WriterT.put[F, L, Unit](())(l)

  def value[F[_], L, V](v: V)(implicit applicativeF: Applicative[F], monoidL: Monoid[L]): WriterT[F, L, V] =
    WriterT.put[F, L, V](v)(monoidL.empty)

  def valueT[F[_], L, V](vf: F[V])(implicit functorF: Functor[F], monoidL: Monoid[L]): WriterT[F, L, V] =
    WriterT.putT[F, L, V](vf)(monoidL.empty)
}
