package cats

import cats.data.{Xor, XorT}

/**
 * An applicative that also allows you to raise and or handle an error value.
 *
 * This type class allows one to abstract over error-handling applicatives.
 */
trait ApplicativeError[F[_], E] extends Applicative[F] {
  /**
   * Lift an error into the `F` context.
   */
  def raiseError[A](e: E): F[A]

  /**
   * Handle any error, potentially recovering from it, by mapping it to an
   * `F[A]` value.
   *
   * @see [[handleError]] to handle any error by simply mapping it to an `A`
   * value instead of an `F[A]`.
   *
   * @see [[recoverWith]] to recover from only certain errors.
   */
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

  /**
   * Handle any error, by mapping it to an `A` value.
   *
   * @see [[handleErrorWith]] to map to an `F[A]` value instead of simply an
   * `A` value.
   *
   * @see [[recover]] to only recover from certain errors.
   */
  def handleError[A](fa: F[A])(f: E => A): F[A] = handleErrorWith(fa)(f andThen pure)

  /**
   * Handle errors by turning them into [[cats.data.Xor.Left]] values.
   *
   * If there is no error, then an [[cats.data.Xor.Right]] value will be returned instead.
   *
   * All non-fatal errors should be handled by this method.
   */
  def attempt[A](fa: F[A]): F[E Xor A] = handleErrorWith(
    map(fa)(Xor.right[E, A])
  )(e => pure(Xor.left(e)))

  /**
   * Similar to [[attempt]], but wraps the result in a [[cats.data.XorT]] for
   * convenience.
   */
  def attemptT[A](fa: F[A]): XorT[F, E, A] = XorT(attempt(fa))

  /**
   * Recover from certain errors by mapping them to an `A` value.
   *
   * @see [[handleError]] to handle any/all errors.
   *
   * @see [[recoverWith]] to recover from certain errors by mapping them to
   * `F[A]` values.
   */
  def recover[A](fa: F[A])(pf: PartialFunction[E, A]): F[A] =
    handleErrorWith(fa)(e =>
      (pf andThen pure) applyOrElse(e, raiseError))

  /**
   * Recover from certain errors by mapping them to an `F[A]` value.
   *
   * @see [[handleErrorWith]] to handle any/all errors.
   *
   * @see [[recover]] to recover from certain errors by mapping them to `A`
   * values.
   */
  def recoverWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A] =
    handleErrorWith(fa)(e =>
      pf applyOrElse(e, raiseError))
}

object ApplicativeError {
  def apply[F[_], E](implicit F: ApplicativeError[F, E]): ApplicativeError[F, E] = F
}
