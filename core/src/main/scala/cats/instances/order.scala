package cats
package instances

trait OrderInstances extends cats.kernel.OrderToOrderingConversion {
  implicit val catsDivisibleForOrder: Divisible[Order] =
    new Divisible[Order] {
      /**
       * Provides trivial order
       */
      def unit[A]: Order[A] = Order.from[A]((x: A, y: A) => 0)
      /** Derive an `Order` for `B` given an `Order[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap2[A, B, C](fb: Order[B], fc: Order[C])(f: A => (B, C)): Order[A] =
        new Order[A] {
          def compare(x: A, y: A): Int =
            (f(x), f(y)) match {
              case (derivedL, derivedR) => {
                val z = fb.compare(derivedL._1, derivedR._1)
                if (z == 0) fc.compare(derivedL._2, derivedR._2) else z
              }
            }
        }
    }
}

