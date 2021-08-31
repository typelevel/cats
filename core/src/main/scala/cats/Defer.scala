package cats

import scala.util.control.TailCalls.TailRec

/**
 * Defer is a type class that shows the ability to defer creation
 * inside of the type constructor F[_].
 *
 * This comes up with F[_] types that are implemented with a trampoline
 * or are based on function application.
 *
 * The law is that defer(fa) is equivalent to fa, but not evaluated immediately,
 * so
 * {{{
 * scala> import cats._
 * scala> import cats.implicits._
 *
 * scala> var evaluated = false
 * scala> val dfa = Defer[Eval].defer {
 *      |   evaluated = true
 *      |   Eval.now(21)
 *      | }
 *
 * scala> evaluated
 * res0: Boolean = false
 *
 * scala> Eq[Eval[Int]].eqv(dfa, Eval.now(21))
 * res1: Boolean = true
 * }}}
 */
trait Defer[F[_]] extends Serializable {
  def defer[A](fa: => F[A]): F[A]

  /**
   * Defer instances, like functions, parsers, generators, IO, etc...
   * often are used in recursive settings where this function is useful
   *
   * fix(fn) == fn(fix(fn))
   *
   * example:
   *
   * val parser: P[Int] =
   *   Defer[P].fix[Int] { rec =>
   *     CharsIn("0123456789") | P("(") ~ rec ~ P(")")
   *   }
   *
   * Note, fn may not yield a terminating value in which case both
   * of the above F[A] run forever.
   */
  def fix[A](fn: F[A] => F[A]): F[A] = {
    lazy val res: F[A] = defer(fn(res))
    res
  }
}

object Defer {
  def apply[F[_]](implicit defer: Defer[F]): Defer[F] = defer

  implicit def catsDeferForFunction0: Defer[Function0] = cats.instances.function.catsSddDeferForFunction0
  implicit def catsDeferForFunction1[A]: Defer[Function1[A, *]] = cats.instances.function.catsStdDeferForFunction1[A]
  implicit def catsDeferForTailRec: Defer[TailRec] = cats.instances.tailRec.catsInstancesForTailRec
}
