package cats
package instances

import cats.functor._

/**
 * @author Tongfei Chen
 */
trait HashInstances {
  implicit val catsFunctorContravariantForHash: Contravariant[Hash] =
    new Contravariant[Hash] {
      /**
       * Derive an `Hash` for `B` given an `Hash[A]` and a function `B => A`.
       */
      def contramap[A, B](ha: Hash[A])(f: B => A): Hash[B] = Hash.by(f)(ha)
    }
}
