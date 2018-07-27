package cats

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
}

object Defer {
  def apply[F[_]](implicit defer: Defer[F]): Defer[F] = defer
}
