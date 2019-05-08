package cats

/**
 * A monad that also allows you to raise and or handle an error value.
 *
 * This type class allows one to abstract over error-handling monads.
 */
trait MonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F] {

  /**
   * Turns a successful value into an error if it does not satisfy a given predicate.
   */
  def ensure[A](fa: F[A])(error: => E)(predicate: A => Boolean): F[A] =
    flatMap(fa)(a => if (predicate(a)) pure(a) else raiseError(error))

  /**
   * Turns a successful value into an error specified by the `error` function if it does not satisfy a given predicate.
   */
  def ensureOr[A](fa: F[A])(error: A => E)(predicate: A => Boolean): F[A] =
    flatMap(fa)(a => if (predicate(a)) pure(a) else raiseError(error(a)))

  /**
   * Transform certain errors using `pf` and rethrow them.
   * Non matching errors and successful values are not affected by this function.
   *
   * Example:
   * {{{
   * scala> import cats._, implicits._
   *
   * scala> def pf: PartialFunction[String, String] = { case "error" => "ERROR" }
   *
   * scala> "error".asLeft[Int].adaptError(pf)
   * res0: Either[String,Int] = Left(ERROR)
   *
   * scala> "err".asLeft[Int].adaptError(pf)
   * res1: Either[String,Int] = Left(err)
   *
   * scala> 1.asRight[String].adaptError(pf)
   * res2: Either[String,Int] = Right(1)
   * }}}
   *
   * The same function is available in `ApplicativeErrorOps` as `adaptErr` - it cannot have the same
   * name because this would result in ambiguous implicits. `adaptError` will be moved from MonadError to
   * ApplicativeError in Cats 2.0: see [[https://github.com/typelevel/cats/issues/2685]]
   */
  def adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A] =
    recoverWith(fa)(pf.andThen(raiseError[A] _))

  /**
   * Inverse of `attempt`
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import scala.util.{Try, Success}
   *
   * scala> val a: Try[Either[Throwable, Int]] = Success(Left(new java.lang.Exception))
   * scala> a.rethrow
   * res0: scala.util.Try[Int] = Failure(java.lang.Exception)
   *
   * scala> val b: Try[Either[Throwable, Int]] = Success(Right(1))
   * scala> b.rethrow
   * res1: scala.util.Try[Int] = Success(1)
   * }}}
   */
  def rethrow[A](fa: F[Either[E, A]]): F[A] =
    flatMap(fa)(_.fold(raiseError, pure))
}

object MonadError {
  def apply[F[_], E](implicit F: MonadError[F, E]): MonadError[F, E] = F
}
