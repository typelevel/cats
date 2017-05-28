package cats
package instances

import cats.functor.Contravariant

trait PartialOrderInstances {
  implicit val catsFunctorContravariantForPartialOrder: Contravariant[PartialOrder] =
    new Contravariant[PartialOrder] {
      /** Derive a `PartialOrder` for `B` given a `PartialOrder[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: PartialOrder[A])(f: B => A): PartialOrder[B] = fa.on(f)
    }
}
