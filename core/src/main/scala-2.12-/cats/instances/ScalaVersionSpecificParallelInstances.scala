package cats
package instances

import cats.data._
import cats.kernel.Semigroup
import cats.syntax.either._
import cats.{~>, Applicative, Apply, FlatMap, Monad, NonEmptyParallel, Parallel}

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

  @deprecated("Use OptionT.catsDataParallelForOptionT", "2.0.0")
  private[instances] def catsParallelForOptionTNestedOption[M[_]](
    implicit P: Parallel[M]
  ): Parallel.Aux[OptionT[M, *], Nested[P.F, Option, *]] = OptionT.catsDataParallelForOptionT[M]

  implicit def catsStdNonEmptyParallelForZipList: NonEmptyParallel.Aux[List, ZipList] =
    new NonEmptyParallel[List] {
      type F[x] = ZipList[x]

      def flatMap: FlatMap[List] = cats.instances.list.catsStdInstancesForList
      def apply: Apply[ZipList] = ZipList.catsDataCommutativeApplyForZipList

      def sequential: ZipList ~> List =
        λ[ZipList ~> List](_.value)

      def parallel: List ~> ZipList =
        λ[List ~> ZipList](v => new ZipList(v))
    }

  implicit def catsStdNonEmptyParallelForZipVector: NonEmptyParallel.Aux[Vector, ZipVector] =
    new NonEmptyParallel[Vector] {
      type F[x] = ZipVector[x]

      def flatMap: FlatMap[Vector] = cats.instances.vector.catsStdInstancesForVector
      def apply: Apply[ZipVector] = ZipVector.catsDataCommutativeApplyForZipVector

      def sequential: ZipVector ~> Vector =
        λ[ZipVector ~> Vector](_.value)

      def parallel: Vector ~> ZipVector =
        λ[Vector ~> ZipVector](v => new ZipVector(v))
    }

  implicit def catsStdParallelForZipStream: Parallel.Aux[Stream, ZipStream] =
    new Parallel[Stream] {
      type F[x] = ZipStream[x]

      def monad: Monad[Stream] = cats.instances.stream.catsStdInstancesForStream
      def applicative: Applicative[ZipStream] = ZipStream.catsDataAlternativeForZipStream

      def sequential: ZipStream ~> Stream =
        λ[ZipStream ~> Stream](_.value)

      def parallel: Stream ~> ZipStream =
        λ[Stream ~> ZipStream](v => new ZipStream(v))
    }

  @deprecated("Use EitherT.catsDataParallelForEitherTWithParallelEffect", "2.0.0")
  private[instances] def catsParallelForEitherTNestedParallelValidated[M[_], E: Semigroup](
    implicit P: Parallel[M]
  ): Parallel.Aux[EitherT[M, E, *], Nested[P.F, Validated[E, *], *]] =
    EitherT.catsDataParallelForEitherTWithParallelEffect[M, E]
}
