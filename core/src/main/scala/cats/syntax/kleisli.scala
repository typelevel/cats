package cats.syntax

import cats.Functor
import cats.data.Kleisli

trait KleisliSyntax {
  implicit final def kleisliSyntax[F[_], A, B](kl: Kleisli[F, A, B]): KleisliOps[F, A, B] = new KleisliOps(kl)
}

final class KleisliOps[F[_], A, B](private val kl: Kleisli[F, A, B]) extends AnyVal {

  /**
   * Yield computed B combined with input value.
   *
   * {{{
   * scala> import cats.data.Kleisli, cats.implicits._
   * scala> val base = Kleisli((_: Int).toString.pure[Either[Throwable, *]])
   * scala> base.tapWithIdentity().run(42)
   * res0: scala.util.Either[Throwable,(Int, String)] = Right((42,42))
   * }}}
   */
  def tapWithIdentity()(implicit F: Functor[F]): Kleisli[F, A, (A, B)] =
    kl.tapWith((a: A, b: B) => (a, b))

}
