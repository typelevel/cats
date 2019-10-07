package cats
package instances

import cats.data._
import cats.kernel.Semigroup
import cats.data.ZipLazyList
import cats.{NonEmptyParallel, Parallel}

trait ParallelInstances extends ParallelInstances1 {

  @deprecated("Use cats.instances.either.catsParallelForEitherAndValidated", "2.1.0")
  def catsParallelForEitherValidated[E: Semigroup]: Parallel.Aux[Either[E, *], Validated[E, *]] =
    cats.instances.either.catsParallelForEitherAndValidated[E]

  @deprecated("Use OptionT.catsDataParallelForOptionT", "2.0.0")
  def catsParallelForOptionTNestedOption[M[_]](
    implicit P: Parallel[M]
  ): Parallel.Aux[OptionT[M, *], Nested[P.F, Option, *]] = OptionT.catsDataParallelForOptionT[M]

  @deprecated("Use cats.instances.list.catsStdNonEmptyParallelForListZipList", "2.1.0")
  def catsStdNonEmptyParallelForZipList: NonEmptyParallel.Aux[List, ZipList] =
    cats.instances.list.catsStdNonEmptyParallelForListZipList

  @deprecated("Use cats.instances.vector.catsStdNonEmptyParallelForVectorZipVector", "2.1.0")
  def catsStdNonEmptyParallelForZipVector: NonEmptyParallel.Aux[Vector, ZipVector] =
    cats.instances.vector.catsStdNonEmptyParallelForVectorZipVector

  @deprecated("Use catsStdParallelForZipLazyList", "2.0.0-RC2")
  def catsStdParallelForZipStream: Parallel.Aux[Stream, ZipStream] =
    cats.instances.stream.catsStdParallelForStreamZipStream

  @deprecated("Use cats.instances.lazyList.catsStdParallelForLazyListZipLazyList", "2.1.0")
  def catsStdParallelForZipLazyList[A]: Parallel.Aux[LazyList, ZipLazyList] =
    cats.instances.lazyList.catsStdParallelForLazyListZipLazyList[A]

  @deprecated("Use EitherT.catsDataParallelForEitherTWithParallelEffect", "2.0.0")
  def catsParallelForEitherTNestedParallelValidated[M[_], E: Semigroup](
    implicit P: Parallel[M]
  ): Parallel.Aux[EitherT[M, E, *], Nested[P.F, Validated[E, *], *]] =
    EitherT.catsDataParallelForEitherTWithParallelEffect[M, E]
}
