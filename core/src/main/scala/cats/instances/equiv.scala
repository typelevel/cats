package cats
package instances

import cats.data.INothing
import cats.instances.either._
import cats.syntax.apply._

trait EquivInstances extends EquivInstances0 {
  implicit val catsDecidableForEquiv: Decidable[Equiv] =
    new Decidable[Equiv] {

      /**
       * Defaults to trivially contracting the type
       * to a point
       */
      def unit: Equiv[Unit] =
        new Equiv[Unit] {
          def equiv(x: Unit, y: Unit): Boolean = true
        }

      /**
       * Derive an `Equiv` for `B` given an `Equiv[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Equiv[A])(f: B => A): Equiv[B] =
        new Equiv[B] {
          def equiv(l: B, r: B): Boolean =
            fa.equiv(f(l), f(r))
        }

      def product[A, B](fa: Equiv[A], fb: Equiv[B]): Equiv[(A, B)] =
        new Equiv[(A, B)] {
          def equiv(l: (A, B), r: (A, B)): Boolean =
            fa.equiv(l._1, r._1) && fb.equiv(l._2, r._2)
        }

      def sum[A, B](fa: Equiv[A], fb: Equiv[B]): Equiv[Either[A, B]] =
        new Equiv[Either[A, B]] {
          def equiv(x: Either[A, B], y: Either[A, B]): Boolean =
            if (x.isRight)
              if (y.isRight)
                (x, y).mapN(fb.equiv).toOption.get
              else false
            else if (y.isLeft)
              (x.swap, y.swap).mapN(fa.equiv).toOption.get
            else false
        }

      override def zero[A]: Equiv[INothing] = Equiv.by(_ => ())
    }
}

private[instances] trait EquivInstances0 {
  implicit def catsContravariantMonoidalForEquiv: ContravariantMonoidal[Equiv] =
    new ContravariantMonoidal[Equiv] {

      /**
       * Defaults to trivially contracting the type
       * to a point
       */
      def unit: Equiv[Unit] =
        new Equiv[Unit] {
          def equiv(x: Unit, y: Unit): Boolean = true
        }

      /**
       * Derive an `Equiv` for `B` given an `Equiv[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Equiv[A])(f: B => A): Equiv[B] =
        new Equiv[B] {
          def equiv(l: B, r: B): Boolean =
            fa.equiv(f(l), f(r))
        }

      def product[A, B](fa: Equiv[A], fb: Equiv[B]): Equiv[(A, B)] =
        new Equiv[(A, B)] {
          def equiv(l: (A, B), r: (A, B)): Boolean =
            fa.equiv(l._1, r._1) && fb.equiv(l._2, r._2)
        }
    }
}
