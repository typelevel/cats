package cats
package data

import cats.Foldable
import cats.kernel.instances.tuple._
import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._

final case class WriterT[F[_], L, V](run: F[(L, V)]) {
  def tell(l: L)(implicit functorF: Functor[F], semigroupL: Semigroup[L]): WriterT[F, L, V] =
    mapWritten(_ |+| l)

  def written(implicit functorF: Functor[F]): F[L] =
    functorF.map(run)(_._1)

  def value(implicit functorF: Functor[F]): F[V] =
    functorF.map(run)(_._2)

  def ap[Z](f: WriterT[F, L, V => Z])(implicit F: Apply[F], L: Semigroup[L]): WriterT[F, L, Z] =
    WriterT(F.map2(f.run, run) {
      case ((l1, fvz), (l2, v)) => (L.combine(l1, l2), fvz(v))
    })

  def map[Z](fn: V => Z)(implicit functorF: Functor[F]): WriterT[F, L, Z] =
    WriterT {
      functorF.map(run) { z =>
        (z._1, fn(z._2))
      }
    }

  def imap[Z](f: V => Z)(g: Z => V)(implicit F: Invariant[F]): WriterT[F, L, Z] =
    WriterT {
      F.imap(run)(z => (z._1, f(z._2)))(z => (z._1, g(z._2)))
    }

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): WriterT[G, L, V] =
    WriterT[G, L, V](f(run))

  def contramap[Z](fn: Z => V)(implicit F: Contravariant[F]): WriterT[F, L, Z] =
    WriterT {
      F.contramap(run) { z =>
        (z._1, fn(z._2))
      }
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

  def foldLeft[C](c: C)(f: (C, V) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(run, c)((z, lv) => f(z, lv._2))

  def foldRight[C](lc: Eval[C])(f: (V, Eval[C]) => Eval[C])(implicit F: Foldable[F]): Eval[C] =
    F.foldRight(run, lc)((lv, z) => f(lv._2, z))

  def traverse[G[_], V1](f: V => G[V1])(implicit F: Traverse[F], G: Applicative[G]): G[WriterT[F, L, V1]] =
    G.map(
      F.traverse(run)(lv => G.tupleLeft(f(lv._2), lv._1))
    )(WriterT.apply)
}

object WriterT extends WriterTInstances with WriterTFunctions {

  def liftF[F[_], L, V](fv: F[V])(implicit monoidL: Monoid[L], F: Applicative[F]): WriterT[F, L, V] =
    WriterT(F.map(fv)(v => (monoidL.empty, v)))

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, ?]]
   * scala> val b: OptionT[WriterT[Eval, String, ?], Int] = a.mapK(WriterT.liftK)
   * scala> b.value.run.value
   * res0: (String, Option[Int]) = ("",Some(1))
   * }}}
   */
  def liftK[F[_], L](implicit monoidL: Monoid[L], F: Applicative[F]): F ~> WriterT[F, L, ?] =
    λ[F ~> WriterT[F, L, ?]](WriterT.liftF(_))

  @deprecated("Use liftF instead", "1.0.0-RC2")
  def lift[F[_], L, V](fv: F[V])(implicit monoidL: Monoid[L], F: Applicative[F]): WriterT[F, L, V] =
    WriterT(F.map(fv)(v => (monoidL.empty, v)))

}

sealed abstract private[data] class WriterTInstances extends WriterTInstances0 {
  implicit def catsDataCommutativeMonadForWriterT[F[_], L](
    implicit F: CommutativeMonad[F],
    L: CommutativeMonoid[L]
  ): CommutativeMonad[WriterT[F, L, ?]] =
    new WriterTMonad[F, L] with CommutativeMonad[WriterT[F, L, ?]] {
      implicit val F0: Monad[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataTraverseForWriterTId[L](implicit F: Traverse[Id]): Traverse[WriterT[Id, L, ?]] =
    catsDataTraverseForWriterT[Id, L](F)

  implicit def catsDataDeferForWriterT[F[_], L](implicit F: Defer[F]): Defer[WriterT[F, L, ?]] =
    new Defer[WriterT[F, L, ?]] {
      def defer[A](fa: => WriterT[F, L, A]): WriterT[F, L, A] =
        WriterT(F.defer(fa.run))
    }
}

sealed abstract private[data] class WriterTInstances0 extends WriterTInstances1 {

  implicit def catsDataTraverseForWriterT[F[_], L](implicit F: Traverse[F]): Traverse[WriterT[F, L, ?]] =
    new WriterTTraverse[F, L] {
      val F0: Traverse[F] = F
    }

  implicit def catsDataFoldableForWriterTId[L](implicit F: Foldable[Id]): Foldable[WriterT[Id, L, ?]] =
    catsDataFoldableForWriterT[Id, L](F)
}

sealed abstract private[data] class WriterTInstances1 extends WriterTInstances2 {
  implicit def catsDataMonadErrorForWriterT[F[_], L, E](implicit F: MonadError[F, E],
                                                        L: Monoid[L]): MonadError[WriterT[F, L, ?], E] =
    new WriterTMonadError[F, L, E] {
      implicit val F0: MonadError[F, E] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataParallelForWriterT[F[_], M[_], L: Monoid](
    implicit P: Parallel[M, F]
  ): Parallel[WriterT[M, L, ?], WriterT[F, L, ?]] = new Parallel[WriterT[M, L, ?], WriterT[F, L, ?]] {
    implicit val appF = P.applicative
    implicit val monadM = P.monad

    def applicative: Applicative[WriterT[F, L, ?]] = catsDataApplicativeForWriterT
    def monad: Monad[WriterT[M, L, ?]] = catsDataMonadForWriterT

    def sequential: WriterT[F, L, ?] ~> WriterT[M, L, ?] =
      λ[WriterT[F, L, ?] ~> WriterT[M, L, ?]](wfl => WriterT(P.sequential(wfl.run)))

    def parallel: WriterT[M, L, ?] ~> WriterT[F, L, ?] =
      λ[WriterT[M, L, ?] ~> WriterT[F, L, ?]](wml => WriterT(P.parallel(wml.run)))
  }

  implicit def catsDataEqForWriterTId[L: Eq, V: Eq]: Eq[WriterT[Id, L, V]] =
    catsDataEqForWriterT[Id, L, V]

  implicit def catsDataBifunctorForWriterT[F[_]: Functor]: Bifunctor[WriterT[F, ?, ?]] =
    new Bifunctor[WriterT[F, ?, ?]] {
      def bimap[A, B, C, D](fab: WriterT[F, A, B])(f: A => C, g: B => D): WriterT[F, C, D] =
        fab.bimap(f, g)
    }

  implicit def catsDataShowForWriterT[F[_], L, V](implicit F: Show[F[(L, V)]]): Show[WriterT[F, L, V]] =
    new Show[WriterT[F, L, V]] {
      override def show(f: WriterT[F, L, V]): String = f.show
    }

  implicit def catsDataMonoidForWriterTId[L: Monoid, V: Monoid]: Monoid[WriterT[Id, L, V]] =
    catsDataMonoidForWriterT[Id, L, V]

  implicit def catsDataFoldableForWriterT[F[_], L](implicit F: Foldable[F]): Foldable[WriterT[F, L, ?]] =
    new WriterTFoldable[F, L] {
      val F0: Foldable[F] = F
    }
}

sealed abstract private[data] class WriterTInstances2 extends WriterTInstances3 {
  implicit def catsDataMonadForWriterTId[L: Monoid]: Monad[WriterT[Id, L, ?]] =
    catsDataMonadForWriterT[Id, L]

  implicit def catsDataEqForWriterT[F[_], L, V](implicit F: Eq[F[(L, V)]]): Eq[WriterT[F, L, V]] =
    Eq.by[WriterT[F, L, V], F[(L, V)]](_.run)

  implicit def catsDataSemigroupForWriterTId[L: Semigroup, V: Semigroup]: Semigroup[WriterT[Id, L, V]] =
    catsDataSemigroupForWriterT[Id, L, V]

  implicit def catsDataComonadForWriterTId[L](implicit F: Comonad[Id]): Comonad[WriterT[Id, L, ?]] =
    catsDataComonadForWriterT[Id, L](F)
}

sealed abstract private[data] class WriterTInstances3 extends WriterTInstances4 {
  implicit def catsDataMonadForWriterT[F[_], L](implicit F: Monad[F], L: Monoid[L]): Monad[WriterT[F, L, ?]] =
    new WriterTMonad[F, L] {
      implicit val F0: Monad[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataMonoidForWriterT[F[_], L, V](implicit W: Monoid[F[(L, V)]]): Monoid[WriterT[F, L, V]] =
    new WriterTMonoid[F, L, V] {
      implicit val F0: Monoid[F[(L, V)]] = W
    }

  implicit def catsDataCoflatMapForWriterTId[L]: CoflatMap[WriterT[Id, L, ?]] =
    catsDataCoflatMapForWriterT[Id, L]
}

sealed abstract private[data] class WriterTInstances4 extends WriterTInstances5 {
  implicit def catsDataFlatMapForWriterTId[L: Semigroup]: FlatMap[WriterT[Id, L, ?]] =
    catsDataFlatMapForWriterT2[Id, L]
}

sealed abstract private[data] class WriterTInstances5 extends WriterTInstances6 {
  implicit def catsDataFlatMapForWriterT1[F[_], L](implicit F: FlatMap[F], L: Monoid[L]): FlatMap[WriterT[F, L, ?]] =
    new WriterTFlatMap1[F, L] {
      implicit val F0: FlatMap[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataSemigroupForWriterT[F[_], L, V](implicit W: Semigroup[F[(L, V)]]): Semigroup[WriterT[F, L, V]] =
    new WriterTSemigroup[F, L, V] {
      implicit val F0: Semigroup[F[(L, V)]] = W
    }
}

sealed abstract private[data] class WriterTInstances6 extends WriterTInstances7 {
  implicit def catsDataApplicativeErrorForWriterT[F[_], L, E](implicit F: ApplicativeError[F, E],
                                                              L: Monoid[L]): ApplicativeError[WriterT[F, L, ?], E] =
    new WriterTApplicativeError[F, L, E] {
      implicit val F0: ApplicativeError[F, E] = F
      implicit val L0: Monoid[L] = L
    }
}

sealed abstract private[data] class WriterTInstances7 extends WriterTInstances8 {
  implicit def catsDataAlternativeForWriterT[F[_], L](implicit F: Alternative[F],
                                                      L: Monoid[L]): Alternative[WriterT[F, L, ?]] =
    new WriterTAlternative[F, L] {
      implicit val F0: Alternative[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataContravariantMonoidalForWriterT[F[_], L](
    implicit F: ContravariantMonoidal[F]
  ): ContravariantMonoidal[WriterT[F, L, ?]] =
    new WriterTContravariantMonoidal[F, L] {
      implicit val F0: ContravariantMonoidal[F] = F
    }
}

sealed abstract private[data] class WriterTInstances8 extends WriterTInstances9 {
  implicit def catsDataMonoidKForWriterT[F[_], L](implicit F: MonoidK[F]): MonoidK[WriterT[F, L, ?]] =
    new WriterTMonoidK[F, L] {
      implicit val F0: MonoidK[F] = F
    }

  implicit def catsDataFlatMapForWriterT2[F[_], L](implicit F: Monad[F], L: Semigroup[L]): FlatMap[WriterT[F, L, ?]] =
    new WriterTFlatMap2[F, L] {
      implicit val F0: Monad[F] = F
      implicit val L0: Semigroup[L] = L
    }

  implicit def catsDataContravariantForWriterT[F[_], L](implicit F: Contravariant[F]): Contravariant[WriterT[F, L, ?]] =
    new WriterTContravariant[F, L] {
      implicit val F0: Contravariant[F] = F
    }
}

sealed abstract private[data] class WriterTInstances9 extends WriterTInstances10 {
  implicit def catsDataSemigroupKForWriterT[F[_], L](implicit F: SemigroupK[F]): SemigroupK[WriterT[F, L, ?]] =
    new WriterTSemigroupK[F, L] {
      implicit val F0: SemigroupK[F] = F
    }

  implicit def catsDataApplicativeForWriterT[F[_], L](implicit F: Applicative[F],
                                                      L: Monoid[L]): Applicative[WriterT[F, L, ?]] =
    new WriterTApplicative[F, L] {
      implicit val F0: Applicative[F] = F
      implicit val L0: Monoid[L] = L
    }

  implicit def catsDataInvariantForWriterT[F[_], L](implicit F0: Invariant[F]): Invariant[WriterT[F, L, ?]] =
    new WriterTInvariant[F, L] { implicit val F = F0 }
}

sealed abstract private[data] class WriterTInstances10 extends WriterTInstances11 {
  implicit def catsDataApplyForWriterT[F[_], L](implicit F: Apply[F], L: Semigroup[L]): Apply[WriterT[F, L, ?]] =
    new WriterTApply[F, L] {
      implicit val F0: Apply[F] = F
      implicit val L0: Semigroup[L] = L
    }
}

sealed abstract private[data] class WriterTInstances11 extends WriterTInstances12 {
  implicit def catsDataComonadForWriterT[F[_], L](implicit F: Comonad[F]): Comonad[WriterT[F, L, ?]] =
    new WriterTComonad[F, L] {
      implicit val F0: Comonad[F] = F
    }
}

sealed abstract private[data] class WriterTInstances12 {
  implicit def catsDataCoflatMapForWriterT[F[_], L](implicit F: Functor[F]): CoflatMap[WriterT[F, L, ?]] =
    new WriterTCoflatMap[F, L] {
      implicit val F0: Functor[F] = F
    }
}

sealed private[data] trait WriterTFunctor[F[_], L] extends Functor[WriterT[F, L, ?]] {
  implicit def F0: Functor[F]

  override def map[A, B](fa: WriterT[F, L, A])(f: A => B): WriterT[F, L, B] =
    fa.map(f)
}

sealed private[data] trait WriterTContravariant[F[_], L] extends Contravariant[WriterT[F, L, ?]] {
  implicit def F0: Contravariant[F]

  override def contramap[A, B](fa: WriterT[F, L, A])(f: B => A): WriterT[F, L, B] =
    fa.contramap(f)
}

sealed private[data] trait WriterTInvariant[F[_], L] extends Invariant[WriterT[F, L, ?]] {
  implicit def F: Invariant[F]

  override def imap[A, B](fa: WriterT[F, L, A])(f: A => B)(g: B => A): WriterT[F, L, B] =
    fa.imap(f)(g)
}

sealed private[data] trait WriterTApply[F[_], L] extends WriterTFunctor[F, L] with Apply[WriterT[F, L, ?]] {
  implicit override def F0: Apply[F]
  implicit def L0: Semigroup[L]

  def ap[A, B](f: WriterT[F, L, A => B])(fa: WriterT[F, L, A]): WriterT[F, L, B] =
    fa.ap(f)

  override def map2Eval[A, B, Z](fa: WriterT[F, L, A],
                                 fb: Eval[WriterT[F, L, B]])(f: (A, B) => Z): Eval[WriterT[F, L, Z]] =
    F0.map2Eval(fa.run, fb.map(_.run)) { case ((la, a), (lb, b)) => (L0.combine(la, lb), f(a, b)) }
      .map(WriterT(_)) // F0 may have a lazy map2Eval

  override def product[A, B](fa: WriterT[F, L, A], fb: WriterT[F, L, B]): WriterT[F, L, (A, B)] =
    WriterT(F0.map(F0.product(fa.run, fb.run)) { case ((l1, a), (l2, b)) => (L0.combine(l1, l2), (a, b)) })
}

sealed private[data] trait WriterTFlatMap1[F[_], L] extends WriterTApply[F, L] with FlatMap[WriterT[F, L, ?]] {
  implicit override def F0: FlatMap[F]
  implicit def L0: Monoid[L]

  override def ap[A, B](f: WriterT[F, L, A => B])(fa: WriterT[F, L, A]): WriterT[F, L, B] =
    super[WriterTApply].ap(f)(fa)

  def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(fn: A => WriterT[F, L, Either[A, B]]): WriterT[F, L, B] = {

    def step(la: (L, A)): F[Either[(L, A), (L, B)]] = {
      val flv: F[(L, Either[A, B])] = fn(la._2).run
      F0.map(flv) {
        case (l, Left(a)) =>
          val combineL = L0.combine(la._1, l)
          Left((combineL, a))
        case (l, Right(b)) =>
          val combineL = L0.combine(la._1, l)
          Right((combineL, b))
      }
    }

    WriterT(F0.tailRecM((L0.empty, a))(step))
  }
}

sealed private[data] trait WriterTFlatMap2[F[_], L] extends WriterTApply[F, L] with FlatMap[WriterT[F, L, ?]] {
  implicit override def F0: Monad[F]
  implicit def L0: Semigroup[L]

  def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(fn: A => WriterT[F, L, Either[A, B]]): WriterT[F, L, B] = {

    def step(la: (L, A)): F[Either[(L, A), (L, B)]] = {
      val flv: F[(L, Either[A, B])] = fn(la._2).run
      F0.map(flv) {
        case (l, Left(a)) =>
          val combineL = L0.combine(la._1, l)
          Left((combineL, a))
        case (l, Right(b)) =>
          val combineL = L0.combine(la._1, l)
          Right((combineL, b))
      }
    }

    val init = fn(a).run
    val res: F[(L, B)] = F0.flatMap(init) {
      case (l, Right(b)) => F0.pure((l, b))
      case (l, Left(a))  => F0.tailRecM((l, a))(step)
    }
    WriterT(res)
  }
}

sealed private[data] trait WriterTApplicative[F[_], L] extends WriterTApply[F, L] with Applicative[WriterT[F, L, ?]] {
  implicit override def F0: Applicative[F]
  implicit override def L0: Monoid[L]

  def pure[A](a: A): WriterT[F, L, A] =
    WriterT.value[F, L, A](a)
}

sealed private[data] trait WriterTMonad[F[_], L]
    extends WriterTApplicative[F, L]
    with WriterTFlatMap1[F, L]
    with Monad[WriterT[F, L, ?]] {
  implicit override def F0: Monad[F]
  implicit override def L0: Monoid[L]
}

sealed private[data] trait WriterTApplicativeError[F[_], L, E]
    extends ApplicativeError[WriterT[F, L, ?], E]
    with WriterTApplicative[F, L] {
  implicit override def F0: ApplicativeError[F, E]

  def raiseError[A](e: E): WriterT[F, L, A] = WriterT(F0.raiseError[(L, A)](e))

  def handleErrorWith[A](fa: WriterT[F, L, A])(f: E => WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.handleErrorWith(fa.run)(e => f(e).run))
}

sealed private[data] trait WriterTMonadError[F[_], L, E]
    extends MonadError[WriterT[F, L, ?], E]
    with WriterTMonad[F, L]
    with WriterTApplicativeError[F, L, E] {
  implicit override def F0: MonadError[F, E]
}

sealed private[data] trait WriterTSemigroupK[F[_], L] extends SemigroupK[WriterT[F, L, ?]] {
  implicit def F0: SemigroupK[F]

  def combineK[A](x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.combineK(x.run, y.run))
}

sealed private[data] trait WriterTMonoidK[F[_], L] extends MonoidK[WriterT[F, L, ?]] with WriterTSemigroupK[F, L] {
  implicit override def F0: MonoidK[F]

  def empty[A]: WriterT[F, L, A] = WriterT(F0.empty)
}

sealed private[data] trait WriterTAlternative[F[_], L]
    extends Alternative[WriterT[F, L, ?]]
    with WriterTMonoidK[F, L]
    with WriterTApplicative[F, L] {
  implicit override def F0: Alternative[F]
}

sealed private[data] trait WriterTContravariantMonoidal[F[_], L] extends ContravariantMonoidal[WriterT[F, L, ?]] {
  implicit def F0: ContravariantMonoidal[F]

  override def unit: WriterT[F, L, Unit] = WriterT(F0.trivial[(L, Unit)])

  override def contramap[A, B](fa: WriterT[F, L, A])(f: B => A): WriterT[F, L, B] =
    fa.contramap(f)

  override def product[A, B](fa: WriterT[F, L, A], fb: WriterT[F, L, B]): WriterT[F, L, (A, B)] =
    WriterT(
      F0.contramap(F0.product(fa.run, fb.run))(
        (t: (L, (A, B))) =>
          t match {
            case (l, (a, b)) => ((l, a), (l, b))
        }
      )
    )
}

sealed private[data] trait WriterTSemigroup[F[_], L, A] extends Semigroup[WriterT[F, L, A]] {
  implicit def F0: Semigroup[F[(L, A)]]

  def combine(x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] =
    WriterT(F0.combine(x.run, y.run))
}

sealed private[data] trait WriterTMonoid[F[_], L, A] extends Monoid[WriterT[F, L, A]] with WriterTSemigroup[F, L, A] {
  implicit override def F0: Monoid[F[(L, A)]]

  def empty: WriterT[F, L, A] = WriterT(F0.empty)
}

sealed private[data] trait WriterTCoflatMap[F[_], L] extends CoflatMap[WriterT[F, L, ?]] with WriterTFunctor[F, L] {

  def coflatMap[A, B](fa: WriterT[F, L, A])(f: WriterT[F, L, A] => B): WriterT[F, L, B] = fa.map(_ => f(fa))
}

sealed private[data] trait WriterTFoldable[F[_], L] extends Foldable[WriterT[F, L, ?]] {

  implicit def F0: Foldable[F]

  def foldLeft[A, B](fa: WriterT[F, L, A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  def foldRight[A, B](fa: WriterT[F, L, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)
}

sealed private[data] trait WriterTTraverse[F[_], L]
    extends Traverse[WriterT[F, L, ?]]
    with WriterTFoldable[F, L]
    with WriterTFunctor[F, L] {

  implicit override def F0: Traverse[F]

  def traverse[G[_]: Applicative, A, B](fa: WriterT[F, L, A])(f: A => G[B]): G[WriterT[F, L, B]] = fa.traverse(f)
}

sealed private[data] trait WriterTComonad[F[_], L] extends Comonad[WriterT[F, L, ?]] with WriterTCoflatMap[F, L] {

  implicit override def F0: Comonad[F]

  def extract[A](fa: WriterT[F, L, A]): A = F0.extract(F0.map(fa.run)(_._2))
}

private[data] trait WriterTFunctions {
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
