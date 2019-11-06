package cats.syntax

import cats.Functor
import cats.data.Kleisli
import cats.syntax.kleisli._

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

  /**
   * Yield computed B combined with input value to passed function,
   * which can change the context of the Kleisli
   * {{{
   * scala> import cats.data.Kleisli, cats.implicits._
   * scala> val base = Kleisli((_: Int).toString.pure[Either[Throwable, *]])
   * scala> base.tapWithMapF(_.toOption).run(42)
   * res0: Option[(Int, String)] = Some((42,42))
   * }}}
   */
  def tapWithMapF[G[_], C](f: F[(A, B)] => G[C])(implicit F: Functor[F]): Kleisli[G, A, C] =
    kl.tapWithIdentity().mapF(f)

}
