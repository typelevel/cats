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
  * `cata`
  *
  * `cata` is a catamorphism over the least fixed point of a the functor `F`
  *
  *
  * remark:
  *
  * `Fix` can be seen as the initial algebra of `F`
  *
  *                algebra
  * F[A] ------------------------> A
  * ^                              ^
  * |                              |
  * | _.map(_.cata(algebra)        | _.cata(algebra)
  * |                              |
  * |              Fix             |
  * F[Fix[F]] -------------------> Fix[F]
  *
  *
  * which, by the way, naturally leads to its definition
  *
  *
  *                algebra
  * F[A] ------------------------> A
  * ^                              ^
  * |                              |
  * | _.map(_.cata(algebra)        | _.cata(algebra)
  * |                              |
  * |              _.unFix         |
  * F[Fix[F]] <------------------- Fix[F]
  *
  *
  * Why is all this useful?
  *
  * `cata` generalizes structural recursion over recursive lists (using `cataRight`)
  * to a whole range of recursive data structures of type `Fix[F]`
  *
  * in order to define a structural recursive function of type `Fix[F] => A`
  * it suffices to define a function (referred to as algebra) of type `F[A] => A`
  *
  * the nice thing about functions of type `F[A] => A` is that they can be defined in a *modular* way
  * because recursion and algebraic operations (sum and product) are strictly separated
  *
  * See [[https://www.researchgate.net/publication/2550340_Using_Catamorphisms_Subtypes_and_Monad_Transformers_for_Writing_Modular_Functional_Interpreters]]
  *
  * for even more information
  *
  * See: [[http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire]]
  *
  * See:[[https://www.schoolofhaskell.com/user/bartosz/understanding-algebras]]
  *
  */

object Types {
   type Algebra[F[_], A] = F[A] => A
 }

import Types._

case class Fix[F[_] : Functor](unFix: F[Fix[F]]) {
  def cata[A](algebra: Algebra[F, A]): A =
    algebra(unFix.map(_.cata(algebra)))
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

  def traverse[A[_] : Applicative, Z, Y](fix: Fix[F[Z, ?]])(z2ay: Z => A[Y]): A[Fix[F[Y, ?]]] = fix.cata(traverseAlgebra(z2ay))
}

