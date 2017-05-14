package cats
package instances

import cats.functor.Contravariant

trait OrderInstances extends cats.kernel.OrderToOrderingConversion {

  implicit val catsFunctorContravariantForOrder: Contravariant[Order] =
    new Contravariant[Order] {
      /** Derive an `Order` for `B` given an `Order[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Order[A])(f: B => A): Order[B] = fa.on(f)
    }
}

