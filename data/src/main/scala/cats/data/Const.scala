package cats.data

import algebra.{Monoid, Order, Semigroup}
import cats.{Applicative, Apply, Lazy, Show, Traverse}

/**
 * [[Const]] is a phantom type, it does not contain a value of its second type parameter `B`
 * [[Const]] can be seen as a type level version of `Function.const[A, B]: A => B => A`
 */
final case class Const[A, B](getConst: A) {
  def retag[C]: Const[A, C] =
    this.asInstanceOf[Const[A, C]]
}

object Const extends ConstInstances

sealed abstract class ConstInstances extends ConstInstances0 {
  implicit def constOrder[A: Order, B]: Order[Const[A, B]] =
    Order.by[Const[A, B], A](_.getConst)

  implicit def constShow[A: Show, B]: Show[Const[A, B]] =
    Show.show[Const[A, B]](c => s"Const(${Show[A].show(c.getConst)}})")

  implicit def constTraverse[C]: Traverse[Const[C, ?]] = new Traverse[Const[C, ?]] {
    def traverse[G[_]: Applicative, A, B](fa: Const[C, A])(f: A => G[B]): G[Const[C, B]] =
      Applicative[G].pure(fa.retag[B])

    def foldLeft[A, B](fa: Const[C, A], b: B)(f: (B, A) => B): B = b

    def foldRight[A, B](fa: Const[C, A], b: B)(f: (A, B) => B): B = b

    def foldRight[A, B](fa: Const[C, A], b: Lazy[B])(f: (A, Lazy[B]) => B): Lazy[B] = b
  }

  implicit def monoidConst[A: Monoid, B]: Monoid[Const[A, B]] = new Monoid[Const[A, B]]{
    def empty: Const[A, B] = Const[A, B](Monoid[A].empty)

    def combine(x: Const[A, B], y: Const[A, B]): Const[A, B] =
      Const[A, B](Monoid[A].combine(x.getConst, y.getConst))
  }
}

sealed abstract class ConstInstances0 extends ConstInstances1 {
  implicit def constApplicative[C: Monoid]: Applicative[Const[C, ?]] = new Applicative[Const[C, ?]] {
    def pure[A](x: A): Const[C, A] =
      Const[C, A](Monoid[C].empty)

    def apply[A, B](fa: Const[C, A])(f: Const[C, A => B]): Const[C, B] =
      fa.retag[B]
  }
}

sealed abstract class ConstInstances1 {
  implicit def constApply[C: Semigroup]: Apply[Const[C, ?]] = new Apply[Const[C, ?]] {
    def apply[A, B](fa: Const[C, A])(f: Const[C, A => B]): Const[C, B] =
      fa.retag[B]

    def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] =
      fa.retag[B]
  }
}
