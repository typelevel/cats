package cats
package instances

import cats.kernel.instances.unit._

trait OrderInstances extends kernel.instances.OrderInstances {

  implicit val catsContravariantMonoidalForOrder: ContravariantMonoidal[Order] =
    new ContravariantMonoidal[Order] {

      /**
       * Provides trivial order
       */
      def unit: Order[Unit] = Order[Unit]

      /**
       * Derive an `Order` for `B` given an `Order[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Order[A])(f: B => A): Order[B] =
        Order.by(f)(fa)

      def product[A, B](fa: Order[A], fb: Order[B]): Order[(A, B)] =
        new Order[(A, B)] {
          def compare(x: (A, B), y: (A, B)): Int = {
            val z = fa.compare(x._1, y._1)
            if (z == 0) fb.compare(x._2, y._2) else z
          }
        }
    }
}
