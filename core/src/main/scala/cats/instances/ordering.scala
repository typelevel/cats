package cats
package instances

import cats.kernel.instances.unit._

trait OrderingInstances {
  implicit val catsContravariantMonoidalForOrdering: ContravariantMonoidal[Ordering] =
    new ContravariantMonoidal[Ordering] {

      /**
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def unit: Ordering[Unit] = Order[Unit].toOrdering

      def contramap[A, B](fa: Ordering[A])(f: B => A): Ordering[B] = fa.on(f)

      def product[A, B](fa: Ordering[A], fb: Ordering[B]): Ordering[(A, B)] =
        new Ordering[(A, B)] {
          def compare(x: (A, B), y: (A, B)): Int = {
            val z = fa.compare(x._1, y._1)
            if (z == 0) fb.compare(x._2, y._2) else z
          }
        }
    }
}
