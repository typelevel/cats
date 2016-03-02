package cats
package io

import cats.data.Xor

import java.lang.Throwable

import scala.reflect.ClassTag

final class IO[A] private[io] (private val eval: Eval[A]) extends Serializable {
  def unsafePerformIO(): A = eval.value

  def catchOnly[T >: Null <: Throwable: ClassTag]: IO[T Xor A] =
    new IO(Eval.always(Xor.catchOnly[T](eval.value)))

  def catchNonFatal: IO[Throwable Xor A] =
    new IO(Eval.always(Xor.catchNonFatal(eval.value)))

  def map[B](f: A => B): IO[B] =
    new IO(eval.map(f))

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO(eval.flatMap(a => f(a).eval))
}

object IO extends IOInstances {
  /** Capture a side-effecting expression as a new IO primitive. */
  def always[A](a: => A): IO[A] =
    new IO[A](Eval.always(a))

  def pure[A](a: A): IO[A] =
    new IO[A](Eval.now(a))
}

trait IOInstances {
  implicit val ioInstances: Monad[IO] =
    new Monad[IO] {
      def pure[A](a: A): IO[A] = IO.pure(a)
      override def pureEval[A](a: Eval[A]): IO[A] = new IO(a)
      def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
      override def map[A, B](ma: IO[A])(f: A => B): IO[B] = ma.map(f)
    }
}
