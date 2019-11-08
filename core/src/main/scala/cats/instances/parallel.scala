package cats
package instances

import cats.data._
import cats.kernel.Semigroup

private[instances] trait ParallelInstances1 {
  @deprecated("Use EitherT.catsDataParallelForEitherTWithSequentialEffect", "2.0.0")
  def catsParallelForEitherTNestedValidated[M[_]: Monad, E: Semigroup]
    : Parallel.Aux[EitherT[M, E, *], Nested[M, Validated[E, *], *]] =
    EitherT.catsDataParallelForEitherTWithSequentialEffect
}
