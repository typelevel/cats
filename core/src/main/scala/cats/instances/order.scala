package cats
package instances

trait OrderInstances extends cats.kernel.OrderToOrderingConversion {

  implicit val catsContravariantSemigroupalForOrder: ContravariantSemigroupal[Order] =
    new ContravariantSemigroupal[Order] {
      /** Derive an `Order` for `B` given an `Order[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Order[A])(f: B => A): Order[B] = fa.on(f)

      def product[A, B](fa: Order[A], fb: Order[B]): Order[(A, B)] =
        new Order[(A, B)] {
          def compare(x: (A, B), y: (A, B)): Int = {
            val z = fa.compare(x._1, y._1)
            if (z == 0) fb.compare(x._2, y._2) else z
          }
        }
    }
}

