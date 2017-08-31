package cats
package instances

import cats.functor._
import cats.kernel.instances._

trait HashInstances {
  implicit val catsContravariantCartesianForHash: ContravariantCartesian[Hash] =
    new ContravariantCartesian[Hash] {
      /**
       * Derive an `Hash` for `B` given an `Hash[A]` and a function `B => A`.
       */
      def contramap[A, B](ha: Hash[A])(f: B => A): Hash[B] = Hash.by(f)(ha)

      /**
       * Derive an `Hash` for `(A, B)` given `Hash[A]` and `Hash[B]`.
       */
      def product[A, B](ha: Hash[A], hb: Hash[B]): Hash[(A, B)] = new Hash[(A, B)] {
        def hash(x: (A, B)) = StaticMethods.product2Hash(ha.hash(x._1), hb.hash(x._2))
        def eqv(x: (A, B), y: (A, B)) = ha.eqv(x._1, y._1) && hb.eqv(x._2, y._2)
      }
    }

}
