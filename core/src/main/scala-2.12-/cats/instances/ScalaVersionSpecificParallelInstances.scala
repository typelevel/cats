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

  implicit def catsParallelForOptionTNestedOption[F0[_], M[_]](
    implicit P: Parallel.Aux[M, F0]
  ): Parallel.Aux[OptionT[M, *], Nested[F0, Option, *]] = new Parallel[OptionT[M, *]] {
    type F[x] = Nested[F0, Option, x]

    implicit val appF: Applicative[F0] = P.applicative
    implicit val monadM: Monad[M] = P.monad
    implicit val appOption: Applicative[Option] = cats.instances.option.catsStdInstancesForOption

    def applicative: Applicative[Nested[F0, Option, *]] = cats.data.Nested.catsDataApplicativeForNested[F0, Option]

    def monad: Monad[OptionT[M, *]] = cats.data.OptionT.catsDataMonadErrorMonadForOptionT[M]

    def sequential: Nested[F0, Option, *] ~> OptionT[M, *] =
      λ[Nested[F0, Option, *] ~> OptionT[M, *]](nested => OptionT(P.sequential(nested.value)))

    def parallel: OptionT[M, *] ~> Nested[F0, Option, *] =
      λ[OptionT[M, *] ~> Nested[F0, Option, *]](optT => Nested(P.parallel(optT.value)))
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

  implicit def catsParallelForEitherTNestedParallelValidated[F0[_], M[_], E: Semigroup](
    implicit P: Parallel.Aux[M, F0]
  ): Parallel.Aux[EitherT[M, E, *], Nested[F0, Validated[E, *], *]] =
    new Parallel[EitherT[M, E, *]] {
      type F[x] = Nested[F0, Validated[E, *], x]

      implicit val appF: Applicative[F0] = P.applicative
      implicit val monadM: Monad[M] = P.monad
      implicit val appValidated: Applicative[Validated[E, *]] = Validated.catsDataApplicativeErrorForValidated
      implicit val monadEither: Monad[Either[E, *]] = cats.instances.either.catsStdInstancesForEither

      def applicative: Applicative[Nested[F0, Validated[E, *], *]] =
        cats.data.Nested.catsDataApplicativeForNested[F0, Validated[E, *]]

      def monad: Monad[EitherT[M, E, *]] = cats.data.EitherT.catsDataMonadErrorForEitherT

      def sequential: Nested[F0, Validated[E, *], *] ~> EitherT[M, E, *] =
        λ[Nested[F0, Validated[E, *], *] ~> EitherT[M, E, *]] { nested =>
          val mva = P.sequential(nested.value)
          EitherT(Functor[M].map(mva)(_.toEither))
        }

      def parallel: EitherT[M, E, *] ~> Nested[F0, Validated[E, *], *] =
        λ[EitherT[M, E, *] ~> Nested[F0, Validated[E, *], *]] { eitherT =>
          val fea = P.parallel(eitherT.value)
          Nested(Functor[F0].map(fea)(_.toValidated))
        }
    }
}
