package cats

/**
 * Defer is a typeclass that shows the ability to defer creation
 * inside of the type constructor F[_].
 *
 * This comes up with F[_] types that are implemented with a trampoline
 * or are based on function application.
 *
 * The law is that defer(fa) is equivalent to fa, but not evaluated immediately,
 * so
 * {{{
 * import cats._
 * import cats.implicits._
 *
 * var evaluated = false
 * val dfa =
 *   Defer[Eval].defer {
 *     evaluated = true
 *     Eval.now(21)
 *    }
 *
 * assert(!evaluated)
 * Eq[Eval[Int]].eqv(dfa, Eval.now(21))
 * }}}
 */
trait Defer[F[_]] extends Serializable {
  def defer[A](fa: => F[A]): F[A]
}

object Defer {
  def apply[F[_]](implicit defer: Defer[F]): Defer[F] = defer
}
