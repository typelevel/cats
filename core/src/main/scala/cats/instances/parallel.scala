package cats.instances

import cats.data._
import cats.kernel.Semigroup
import cats.syntax.either._
import cats.{Applicative, Functor, Monad, Parallel, ~>}


trait ParallelInstances extends ParallelInstances1 {
  implicit def catsParallelForEitherValidated[E: Semigroup]: Parallel[Either[E, ?], Validated[E, ?]] = new Parallel[Either[E, ?], Validated[E, ?]] {
    def applicative: Applicative[Validated[E, ?]] = Validated.catsDataApplicativeErrorForValidated

    def sequential(implicit M: Monad[Either[E, ?]]): Validated[E, ?] ~> Either[E, ?] =
      λ[Validated[E, ?] ~> Either[E, ?]](_.toEither)

    def parallel(implicit M: Monad[Either[E, ?]]): Either[E, ?] ~> Validated[E, ?] =
      λ[Either[E, ?] ~> Validated[E, ?]](_.toValidated)
  }

  implicit def catsParallelForOptionTNestedOption[F[_], M[_]: Monad]
  (implicit P: Parallel[M, F]): Parallel[OptionT[M, ?], Nested[F, Option, ?]] = new Parallel[OptionT[M, ?], Nested[F, Option, ?]] {
    implicit val appF: Applicative[F] = P.applicative
    implicit val appOption: Applicative[Option] = cats.instances.option.catsStdInstancesForOption

    def applicative: Applicative[Nested[F, Option, ?]] = cats.data.Nested.catsDataApplicativeForNested[F, Option]

    def sequential(implicit M: Monad[OptionT[M, ?]]): Nested[F, Option, ?] ~> OptionT[M, ?] =
      λ[Nested[F, Option, ?] ~> OptionT[M, ?]](nested => OptionT(P.sequential.apply(nested.value)))

    def parallel(implicit M: Monad[OptionT[M, ?]]): OptionT[M, ?]~> Nested[F, Option, ?] =
      λ[OptionT[M, ?] ~> Nested[F, Option, ?]](optT => Nested(P.parallel.apply(optT.value)))
  }

  implicit def catsStdParallelForZipVector[A]: Parallel[Vector, ZipVector] =
    new Parallel[Vector, ZipVector] {

      def applicative: Applicative[ZipVector] = ZipVector.catsDataApplicativeForZipVector

      def sequential(implicit M: Monad[Vector]): ZipVector ~> Vector =
        λ[ZipVector ~> Vector](_.value)

      def parallel(implicit M: Monad[Vector]): Vector ~> ZipVector =
        λ[Vector ~> ZipVector](v => new ZipVector(v))
    }

  implicit def catsParallelForEitherTNestedParallelValidated[F[_], M[_]: Monad, E: Semigroup]
  (implicit P: Parallel[M, F]): Parallel[EitherT[M, E, ?], Nested[F, Validated[E, ?], ?]] =
    new Parallel[EitherT[M, E, ?], Nested[F, Validated[E, ?], ?]] {

    implicit val appF: Applicative[F] = P.applicative
    implicit val appValidated: Applicative[Validated[E, ?]] = Validated.catsDataApplicativeErrorForValidated

    def applicative: Applicative[Nested[F, Validated[E, ?], ?]] = cats.data.Nested.catsDataApplicativeForNested[F, Validated[E, ?]]

    def sequential(implicit M: Monad[EitherT[M, E, ?]]): Nested[F, Validated[E, ?], ?] ~> EitherT[M, E, ?] =
      λ[Nested[F, Validated[E, ?], ?] ~> EitherT[M, E, ?]] { nested =>
        val mva = P.sequential.apply(nested.value)
        EitherT(Functor[M].map(mva)(_.toEither))
      }

    def parallel(implicit M: Monad[EitherT[M, E, ?]]): EitherT[M, E, ?]~> Nested[F, Validated[E, ?], ?] =
      λ[EitherT[M, E, ?] ~> Nested[F, Validated[E, ?], ?]] { eitherT =>
        val fea = P.parallel.apply(eitherT.value)
        Nested(Functor[F].map(fea)(_.toValidated))
      }
  }
}

private[instances] trait ParallelInstances1 {
  implicit def catsParallelForEitherTNestedValidated[M[_]: Monad, E: Semigroup]: Parallel[EitherT[M, E, ?], Nested[M, Validated[E, ?], ?]] =
    new Parallel[EitherT[M, E, ?], Nested[M, Validated[E, ?], ?]] {

      implicit val appValidated: Applicative[Validated[E, ?]] = Validated.catsDataApplicativeErrorForValidated

      def applicative: Applicative[Nested[M, Validated[E, ?], ?]] = cats.data.Nested.catsDataApplicativeForNested[M, Validated[E, ?]]

      def sequential(implicit M: Monad[EitherT[M, E, ?]]): Nested[M, Validated[E, ?], ?] ~> EitherT[M, E, ?] =
        λ[Nested[M, Validated[E, ?], ?] ~> EitherT[M, E, ?]] { nested =>
          EitherT(Functor[M].map(nested.value)(_.toEither))
        }

      def parallel(implicit M: Monad[EitherT[M, E, ?]]): EitherT[M, E, ?]~> Nested[M, Validated[E, ?], ?] =
        λ[EitherT[M, E, ?] ~> Nested[M, Validated[E, ?], ?]] { eitherT =>
          Nested(Functor[M].map(eitherT.value)(_.toValidated))
        }
    }
}
