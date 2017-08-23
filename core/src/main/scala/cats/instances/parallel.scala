package cats.instances

import cats.data.{EitherT, Nested, OptionT, Validated}
import cats.kernel.Semigroup
import cats.syntax.either._
import cats.{Applicative, Monad, Parallel, ~>}


trait ParallelInstances {
  implicit def parEitherValidated[E: Semigroup]: Parallel[Either[E, ?], Validated[E, ?]] = new Parallel[Either[E, ?], Validated[E, ?]] {
    def applicative: Applicative[Validated[E, ?]] = Validated.catsDataApplicativeErrorForValidated

    def sequential(implicit M: Monad[Either[E, ?]]): Validated[E, ?] ~> Either[E, ?] =
      λ[Validated[E, ?] ~> Either[E, ?]](_.toEither)

    def parallel(implicit M: Monad[Either[E, ?]]): Either[E, ?] ~> Validated[E, ?] =
      λ[Either[E, ?] ~> Validated[E, ?]](_.toValidated)
  }

  implicit def catsDataParallelForOptionT[F[_], M[_]: Monad]
  (implicit P: Parallel[M, F]): Parallel[OptionT[M, ?], Nested[F, Option, ?]] = new Parallel[OptionT[M, ?], Nested[F, Option, ?]] {
    implicit val appF: Applicative[F] = P.applicative
    implicit val appOption: Applicative[Option] = cats.instances.option.catsStdInstancesForOption

    def applicative: Applicative[Nested[F, Option, ?]] = cats.data.Nested.catsDataApplicativeForNested[F, Option]

    def sequential(implicit M: Monad[OptionT[M, ?]]): Nested[F, Option, ?] ~> OptionT[M, ?] =
      λ[Nested[F, Option, ?] ~> OptionT[M, ?]](nested => OptionT(P.sequential.apply(nested.value)))

    def parallel(implicit M: Monad[OptionT[M, ?]]): OptionT[M, ?]~> Nested[F, Option, ?] =
      λ[OptionT[M, ?] ~> Nested[F, Option, ?]](optT => Nested(P.parallel.apply(optT.value)))
  }

  implicit def catsDataParallelForEitherT[F[_], M[_]: Monad, E]
  (implicit P: Parallel[M, F]): Parallel[EitherT[M, E, ?], Nested[F, Either[E, ?], ?]] =
    new Parallel[EitherT[M, E, ?], Nested[F, Either[E, ?], ?]] {

    implicit val appF: Applicative[F] = P.applicative
    implicit val appOption: Applicative[Either[E, ?]] = cats.instances.either.catsStdInstancesForEither

    def applicative: Applicative[Nested[F, Either[E, ?], ?]] = cats.data.Nested.catsDataApplicativeForNested[F, Either[E, ?]]

    def sequential(implicit M: Monad[EitherT[M, E, ?]]): Nested[F, Either[E, ?], ?] ~> EitherT[M, E, ?] =
      λ[Nested[F, Either[E, ?], ?] ~> EitherT[M, E, ?]](nested => EitherT(P.sequential.apply(nested.value)))

    def parallel(implicit M: Monad[EitherT[M, E, ?]]): EitherT[M, E, ?]~> Nested[F, Either[E, ?], ?] =
      λ[EitherT[M, E, ?] ~> Nested[F, Either[E, ?], ?]](eitherT => Nested(P.parallel.apply(eitherT.value)))
  }
}
