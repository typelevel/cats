package cats
package instances

import cats.functor.Contravariant

trait OrderingInstances {

  implicit val catsFunctorContravariantForOrdering: Contravariant[Ordering] =
    new Contravariant[Ordering] {
      /** Derive an `Ordering` for `B` given an `Ordering[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Ordering[A])(f: B => A): Ordering[B] = fa.on(f)
    }
}
