package cats
package functor

import simulacrum._

/**
 * Must obey the laws defined in [[laws.InvariantLaws]].
 */
@typeclass trait Invariant[F[_]] extends Any {
  def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B]
}
