package cats
package instances

import cats.data.INothing
import cats.instances.either._
import cats.syntax.apply._
import cats.kernel.instances.unit._

trait OrderInstances extends kernel.instances.OrderInstances {
  implicit val catsDecidableForOrder: Decidable[Order] =
    new Decidable[Order] {

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

      def sum[A, B](fa: Order[A], fb: Order[B]): Order[Either[A, B]] =
        new Order[Either[A, B]] {
          def compare(x: Either[A, B], y: Either[A, B]): Int =
            if (x.isRight)
              if (y.isRight)
                (x, y).mapN(fb.compare).toOption.get
              else 1
            else if (y.isLeft)
              (x.swap, y.swap).mapN(fa.compare).toOption.get
            else -1
        }

      override def zero[A]: Order[INothing] = Order.by(_ => ())
    }
}

private [instances] trait OrderInstancesBinCompat0 {
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
