package cats.fix

import cats.{Applicative, Traverse, Functor}
import cats.syntax.functor._

/**
  * `Fix`
  *
  * `Fix[F]` is the least fixed point of the functor `F`
  *
  *
  * `Algebra`
  *
  * a function of type `F[A] => A` is an algebra of the functor `F`
  *
  *
  * `fold`
  *
  * `fold` is a catamorphism over the least fixed point of a the functor `F`
  *
  *
  * remark:
  *
  * `Fix` can be seen as the initial algebra of `F`
  *
  *                algebra
  *    F[A] ------------------------> A
  *    ^                              ^
  *    |                              |
  *    | _.map(_.fold(algebra)        | _.fold(algebra)
  *    |                              |
  *    |              Fix             |
  *    F[Fix[F]] -------------------> Fix[F]
  *
  *
  * which, by the way, naturally leads to its definition
  *
  *
  *                algebra
  *    F[A] ------------------------> A
  *    ^                              ^
  *    |                              |
  *    | _.map(_.fold(algebra)        | _.fold(algebra)
  *    |                              |
  *    |              unFix           |
  *    F[Fix[F]] <------------------- Fix[F]
  *
  *
  * See: [[http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire]]
  *
  */

case class Fix[F[_] : Functor](unFix: F[Fix[F]]) {
  def fold[A](algebra: F[A] => A): A =
    algebra(unFix.map(_.fold(algebra)))
}

/**
  * `FixTraverse`
  *
  * in order to traverse the least fixed point of a functor
  * it suffices to define an appropriate algebra
  *
  */

trait FixTraverse[F[_, _]] extends Traverse[({type λ[Z] = Fix[F[Z, ?]]})#λ] {
  def traverseAlgebra[A[_] : Applicative, Z, Y](z2ay: Z => A[Y]): F[Z, A[Fix[F[Y, ?]]]] => A[Fix[F[Y, ?]]]
  def traverse[A[_] : Applicative, Z, Y](fix: Fix[F[Z, ?]])(z2ay: Z => A[Y]): A[Fix[F[Y, ?]]] = fix.fold(traverseAlgebra(z2ay))
}

