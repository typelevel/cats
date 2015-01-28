package cats
package functor

import simulacrum._

@typeclass trait Invariant[F[_]] extends Any {
  def imap[A, B](fa: F[A])(f: A <=> B): F[B]
}
