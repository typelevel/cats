package cats
package instances

trait PartialOrderInstances extends kernel.instances.PartialOrderInstances {
  implicit val catsContravariantSemigroupalForPartialOrder: ContravariantSemigroupal[PartialOrder] =
    new ContravariantSemigroupal[PartialOrder] {
      /** Derive a `PartialOrder` for `B` given a `PartialOrder[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: PartialOrder[A])(f: B => A): PartialOrder[B] = PartialOrder.by[B, A](f)(fa)

      def product[A, B](fa: PartialOrder[A], fb: PartialOrder[B]): PartialOrder[(A, B)] =
        new PartialOrder[(A, B)] {
          def partialCompare(x: (A, B), y: (A, B)): Double = {
            val z = fa.partialCompare(x._1, y._1)
            if (z == 0.0) fb.partialCompare(x._2, y._2) else z
          }
        }
    }
}
