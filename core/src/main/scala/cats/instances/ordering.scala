package cats
package instances

trait OrderingInstances {
  implicit val catsDivisibleForOrdering: Divisible[Ordering] =
    new Divisible[Ordering] {
      /**
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */

      def unit[A]: Ordering[A] = new Ordering[A] {
        def compare(l: A, r: A): Int = 0
      }

      def contramap2[A, B, C](fb: Ordering[B], fc: Ordering[C])(f: A => (B, C)): Ordering[A] =
        new Ordering[A] {
          def compare(x: A, y: A): Int = (f(x), f(y)) match {
            case ((bL, cL), (bR, cR)) => {
              val z = fb.compare(bL, bR)
              if (z == 0) fc.compare(cL, cR) else z
            }
          }
        }
    }
}
