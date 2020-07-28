package cats
package instances

import cats.data.INothing
import cats.instances.either._
import cats.syntax.apply._

trait EqInstances extends kernel.instances.EqInstances with EqInstances0 {
  implicit def catsDecidableForEq: Decidable[Eq] =
    new Decidable[Eq] {

      /**
       * Defaults to the trivial equivalence relation
       * contracting the type to a point
       */
      def unit: Eq[Unit] = Eq.allEqual

      /**
       * Derive an `Eq` for `B` given an `Eq[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Eq[A])(f: B => A): Eq[B] =
        Eq.by(f)(fa)

      def product[A, B](fa: Eq[A], fb: Eq[B]): Eq[(A, B)] =
        Eq.instance { (left, right) =>
          fa.eqv(left._1, right._1) && fb.eqv(left._2, right._2)
        }

      def sum[A, B](fa: Eq[A], fb: Eq[B]): Eq[Either[A, B]] =
        Eq.instance { (left, right) =>
          if (left.isRight)
            if (right.isRight)
              (left, right).mapN(fb.eqv).toOption.get
            else false
          else if (right.isLeft)
            (left.swap, right.swap).mapN(fa.eqv).toOption.get
          else false
        }

      override def zero[A]: Eq[INothing] = Eq.allEqual[INothing]
    }
}

private[instances] trait EqInstances0 {
  implicit def catsContravariantMonoidalForEq: ContravariantMonoidal[Eq] =
    new ContravariantMonoidal[Eq] {

      /**
       * Defaults to the trivial equivalence relation
       * contracting the type to a point
       */
      def unit: Eq[Unit] = Eq.allEqual

      /**
       * Derive an `Eq` for `B` given an `Eq[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Eq[A])(f: B => A): Eq[B] =
        Eq.by(f)(fa)

      def product[A, B](fa: Eq[A], fb: Eq[B]): Eq[(A, B)] =
        Eq.instance { (left, right) =>
          fa.eqv(left._1, right._1) && fb.eqv(left._2, right._2)
        }
    }
}
