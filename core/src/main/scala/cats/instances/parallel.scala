package cats.instances

import cats.data.Validated
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
}
