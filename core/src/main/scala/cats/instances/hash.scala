package cats
package instances

import cats.functor._

/**
 * @author Tongfei Chen
 */
trait HashInstances {
  implicit val catsFunctorContravariantForHash: Contravariant[Hash] =
    new Contravariant[Hash] {
      /** Derive an `Hash` for `B` given an `Hash[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Hash[A])(f: B => A): Hash[B] = Hash.by(f)(fa)
    }
}
