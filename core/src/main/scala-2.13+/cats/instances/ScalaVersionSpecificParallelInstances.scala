package cats
package instances

import cats.data._
import cats.kernel.Semigroup
import cats.syntax.either._
import cats.{~>, Applicative, Apply, FlatMap, Functor, Monad, NonEmptyParallel, Parallel}

trait ParallelInstances extends ParallelInstances1 {
  implicit def catsParallelForEitherValidated[E: Semigroup]: Parallel.Aux[Either[E, *], Validated[E, *]] =
    new Parallel[Either[E, *]] {
      type F[x] = Validated[E, x]

      def applicative: Applicative[Validated[E, *]] = Validated.catsDataApplicativeErrorForValidated
      def monad: Monad[Either[E, *]] = cats.instances.either.catsStdInstancesForEither

      def sequential: Validated[E, *] ~> Either[E, *] =
        λ[Validated[E, *] ~> Either[E, *]](_.toEither)

      def parallel: Either[E, *] ~> Validated[E, *] =
        λ[Either[E, *] ~> Validated[E, *]](_.toValidated)
    }

  implicit def catsParallelForOptionTNestedOption[M[_]](
    implicit P: Parallel[M]
  ): Parallel.Aux[OptionT[M, *], Nested[P.F, Option, *]] = new Parallel[OptionT[M, *]] {
    type F[x] = Nested[P.F, Option, x]

    implicit val monadM: Monad[M] = P.monad

    def applicative: Applicative[Nested[P.F, Option, *]] =
      cats.data.Nested.catsDataApplicativeForNested(P.applicative, cats.instances.option.catsStdInstancesForOption)

    def monad: Monad[OptionT[M, *]] = cats.data.OptionT.catsDataMonadErrorMonadForOptionT[M]

    def sequential: Nested[P.F, Option, *] ~> OptionT[M, *] =
      λ[Nested[P.F, Option, *] ~> OptionT[M, *]](nested => OptionT(P.sequential(nested.value)))

    def parallel: OptionT[M, *] ~> Nested[P.F, Option, *] =
      λ[OptionT[M, *] ~> Nested[P.F, Option, *]](optT => Nested(P.parallel(optT.value)))
  }

  implicit def catsStdNonEmptyParallelForZipList[A]: NonEmptyParallel.Aux[List, ZipList] =
    new NonEmptyParallel[List] {
      type F[x] = ZipList[x]

      def flatMap: FlatMap[List] = cats.instances.list.catsStdInstancesForList
      def apply: Apply[ZipList] = ZipList.catsDataCommutativeApplyForZipList

      def sequential: ZipList ~> List =
        λ[ZipList ~> List](_.value)

      def parallel: List ~> ZipList =
        λ[List ~> ZipList](v => new ZipList(v))
    }

  implicit def catsStdNonEmptyParallelForZipVector[A]: NonEmptyParallel.Aux[Vector, ZipVector] =
    new NonEmptyParallel[Vector] {
      type F[x] = ZipVector[x]

      def flatMap: FlatMap[Vector] = cats.instances.vector.catsStdInstancesForVector
      def apply: Apply[ZipVector] = ZipVector.catsDataCommutativeApplyForZipVector

      def sequential: ZipVector ~> Vector =
        λ[ZipVector ~> Vector](_.value)

      def parallel: Vector ~> ZipVector =
        λ[Vector ~> ZipVector](v => new ZipVector(v))
    }

  @deprecated("Use catsStdParallelForZipLazyList", "2.0.0-RC2")
  implicit def catsStdParallelForZipStream[A]: Parallel.Aux[Stream, ZipStream] =
    new Parallel[Stream] {
      type F[x] = ZipStream[x]

      def monad: Monad[Stream] = cats.instances.stream.catsStdInstancesForStream
      def applicative: Applicative[ZipStream] = ZipStream.catsDataAlternativeForZipStream

      def sequential: ZipStream ~> Stream =
        λ[ZipStream ~> Stream](_.value)

      def parallel: Stream ~> ZipStream =
        λ[Stream ~> ZipStream](v => new ZipStream(v))
    }

  implicit def catsStdParallelForZipLazyList[A]: Parallel.Aux[LazyList, ZipLazyList] =
    new Parallel[LazyList] {
      type F[x] = ZipLazyList[x]

      def monad: Monad[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
      def applicative: Applicative[ZipLazyList] = ZipLazyList.catsDataAlternativeForZipLazyList

      def sequential: ZipLazyList ~> LazyList =
        λ[ZipLazyList ~> LazyList](_.value)

      def parallel: LazyList ~> ZipLazyList =
        λ[LazyList ~> ZipLazyList](v => new ZipLazyList(v))
    }

  implicit def catsParallelForEitherTNestedParallelValidated[M[_], E: Semigroup](
    implicit P: Parallel[M]
  ): Parallel.Aux[EitherT[M, E, *], Nested[P.F, Validated[E, *], *]] =
    new Parallel[EitherT[M, E, *]] {
      type F[x] = Nested[P.F, Validated[E, *], x]

      implicit val monadM: Monad[M] = P.monad
      implicit val monadEither: Monad[Either[E, *]] = cats.instances.either.catsStdInstancesForEither

      def applicative: Applicative[Nested[P.F, Validated[E, *], *]] =
        cats.data.Nested.catsDataApplicativeForNested(P.applicative, Validated.catsDataApplicativeErrorForValidated)

      def monad: Monad[EitherT[M, E, *]] = cats.data.EitherT.catsDataMonadErrorForEitherT

      def sequential: Nested[P.F, Validated[E, *], *] ~> EitherT[M, E, *] =
        λ[Nested[P.F, Validated[E, *], *] ~> EitherT[M, E, *]] { nested =>
          val mva = P.sequential(nested.value)
          EitherT(Functor[M].map(mva)(_.toEither))
        }

      def parallel: EitherT[M, E, *] ~> Nested[P.F, Validated[E, *], *] =
        λ[EitherT[M, E, *] ~> Nested[P.F, Validated[E, *], *]] { eitherT =>
          val fea = P.parallel(eitherT.value)
          Nested(P.applicative.map(fea)(_.toValidated))
        }
    }
}
