package cats
package instances

import cats.data._
import cats.kernel.Semigroup
import cats.syntax.either._
import cats.{~>, Applicative, Monad, Parallel}

private[instances] trait ParallelInstances1 {
  @deprecated("Use EitherT.catsDataParallelForEitherTWithSequentialEffect", "2.0.0")
  private[instances] def catsParallelForEitherTNestedValidated[M[_]: Monad, E: Semigroup]
    : Parallel.Aux[EitherT[M, E, *], Nested[M, Validated[E, *], *]] =
    new Parallel[EitherT[M, E, *]] {
      type F[x] = Nested[M, Validated[E, *], x]

      implicit val appValidated: Applicative[Validated[E, *]] = Validated.catsDataApplicativeErrorForValidated
      implicit val monadEither: Monad[Either[E, *]] = cats.instances.either.catsStdInstancesForEither

      def applicative: Applicative[Nested[M, Validated[E, *], *]] =
        cats.data.Nested.catsDataApplicativeForNested[M, Validated[E, *]]

      def monad: Monad[EitherT[M, E, *]] = cats.data.EitherT.catsDataMonadErrorForEitherT

      def sequential: Nested[M, Validated[E, *], *] ~> EitherT[M, E, *] =
        λ[Nested[M, Validated[E, *], *] ~> EitherT[M, E, *]] { nested =>
          EitherT(Monad[M].map(nested.value)(_.toEither))
        }

      def parallel: EitherT[M, E, *] ~> Nested[M, Validated[E, *], *] =
        λ[EitherT[M, E, *] ~> Nested[M, Validated[E, *], *]] { eitherT =>
          Nested(Monad[M].map(eitherT.value)(_.toValidated))
        }
    }
}
