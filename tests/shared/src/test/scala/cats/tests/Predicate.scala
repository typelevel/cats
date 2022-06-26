package cats.tests

import cats.Decidable
import cats.data.INothing
import cats.kernel.Eq
import cats.laws.discipline.eq._
import cats.laws.discipline.ExhaustiveCheck
import org.scalacheck.{Arbitrary, Cogen}

case class Predicate[A](run: A => Boolean)
object Predicate {
  implicit val decidablePredicate: Decidable[Predicate] =
    new Decidable[Predicate] {
      def unit: Predicate[Unit] = Predicate[Unit](Function.const(false))
      def product[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[(A, B)] =
        Predicate(x => fa.run(x._1) || fb.run(x._2))
      def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] =
        Predicate(x => fa.run(f(x)))
      def sum[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[Either[A, B]] =
        Predicate(_.fold(fa.run, fb.run))
      def zero[A]: Predicate[INothing] = Predicate(_ => true)
    }

  implicit def eqPredicate[A: ExhaustiveCheck]: Eq[Predicate[A]] =
    Eq.by[Predicate[A], A => Boolean](_.run)

  implicit def arbPredicate[A: Cogen]: Arbitrary[Predicate[A]] =
    Arbitrary(implicitly[Arbitrary[A => Boolean]].arbitrary.map(f => Predicate(f)))
}
