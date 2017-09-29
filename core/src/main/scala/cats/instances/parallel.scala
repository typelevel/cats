package cats.instances

import cats.data._
import cats.kernel.Semigroup
import cats.syntax.either._
import cats.{Applicative, Functor, Monad, Parallel, ~>}

trait ParallelInstances extends ParallelInstances1 {
  implicit def catsParallelForEitherValidated[E: Semigroup]: Parallel[Either[E, ?], Validated[E, ?]] = new Parallel[Either[E, ?], Validated[E, ?]] {

    def applicative: Applicative[Validated[E, ?]] = Validated.catsDataApplicativeErrorForValidated
    def monad: Monad[Either[E, ?]] = cats.instances.either.catsStdInstancesForEither

    def sequential: Validated[E, ?] ~> Either[E, ?] =
      λ[Validated[E, ?] ~> Either[E, ?]](_.toEither)

    def parallel: Either[E, ?] ~> Validated[E, ?] =
      λ[Either[E, ?] ~> Validated[E, ?]](_.toValidated)
  }

  implicit def catsParallelForOptionTNestedOption[F[_], M[_]]
  (implicit P: Parallel[M, F]): Parallel[OptionT[M, ?], Nested[F, Option, ?]] = new Parallel[OptionT[M, ?], Nested[F, Option, ?]] {

    implicit val appF: Applicative[F] = P.applicative
    implicit val monadM: Monad[M] = P.monad
    implicit val appOption: Applicative[Option] = cats.instances.option.catsStdInstancesForOption

    def applicative: Applicative[Nested[F, Option, ?]] = cats.data.Nested.catsDataApplicativeForNested[F, Option]

    def monad: Monad[OptionT[M, ?]] = cats.data.OptionT.catsDataMonadForOptionT[M]

    def sequential: Nested[F, Option, ?] ~> OptionT[M, ?] =
      λ[Nested[F, Option, ?] ~> OptionT[M, ?]](nested => OptionT(P.sequential(nested.value)))

    def parallel: OptionT[M, ?]~> Nested[F, Option, ?] =
      λ[OptionT[M, ?] ~> Nested[F, Option, ?]](optT => Nested(P.parallel(optT.value)))
  }


  implicit def catsParallelForEitherTNestedParallelValidated[F[_], M[_], E: Semigroup]
  (implicit P: Parallel[M, F]): Parallel[EitherT[M, E, ?], Nested[F, Validated[E, ?], ?]] =
    new Parallel[EitherT[M, E, ?], Nested[F, Validated[E, ?], ?]] {

    implicit val appF: Applicative[F] = P.applicative
    implicit val monadM: Monad[M] = P.monad
    implicit val appValidated: Applicative[Validated[E, ?]] = Validated.catsDataApplicativeErrorForValidated
    implicit val monadEither: Monad[Either[E, ?]] = cats.instances.either.catsStdInstancesForEither

    def applicative: Applicative[Nested[F, Validated[E, ?], ?]] = cats.data.Nested.catsDataApplicativeForNested[F, Validated[E, ?]]

    def monad: Monad[EitherT[M, E, ?]] = cats.data.EitherT.catsDataMonadErrorForEitherT

    def sequential: Nested[F, Validated[E, ?], ?] ~> EitherT[M, E, ?] =
      λ[Nested[F, Validated[E, ?], ?] ~> EitherT[M, E, ?]] { nested =>
        val mva = P.sequential(nested.value)
        EitherT(Functor[M].map(mva)(_.toEither))
      }

    def parallel: EitherT[M, E, ?]~> Nested[F, Validated[E, ?], ?] =
      λ[EitherT[M, E, ?] ~> Nested[F, Validated[E, ?], ?]] { eitherT =>
        val fea = P.parallel(eitherT.value)
        Nested(Functor[F].map(fea)(_.toValidated))
      }
  }
}

private[instances] trait ParallelInstances1 {
  implicit def catsParallelForEitherTNestedValidated[M[_]: Monad, E: Semigroup]: Parallel[EitherT[M, E, ?], Nested[M, Validated[E, ?], ?]] =
    new Parallel[EitherT[M, E, ?], Nested[M, Validated[E, ?], ?]] {

      implicit val appValidated: Applicative[Validated[E, ?]] = Validated.catsDataApplicativeErrorForValidated
      implicit val monadEither: Monad[Either[E, ?]] = cats.instances.either.catsStdInstancesForEither

      def applicative: Applicative[Nested[M, Validated[E, ?], ?]] = cats.data.Nested.catsDataApplicativeForNested[M, Validated[E, ?]]

      def monad: Monad[EitherT[M, E, ?]] = cats.data.EitherT.catsDataMonadErrorForEitherT

      def sequential: Nested[M, Validated[E, ?], ?] ~> EitherT[M, E, ?] =
        λ[Nested[M, Validated[E, ?], ?] ~> EitherT[M, E, ?]] { nested =>
          EitherT(Monad[M].map(nested.value)(_.toEither))
        }

      def parallel: EitherT[M, E, ?]~> Nested[M, Validated[E, ?], ?] =
        λ[EitherT[M, E, ?] ~> Nested[M, Validated[E, ?], ?]] { eitherT =>
          Nested(Monad[M].map(eitherT.value)(_.toValidated))
        }
    }
}
