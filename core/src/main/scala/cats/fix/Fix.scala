package cats.fix

import cats.{Applicative, Traverse, Functor}
import cats.syntax.functor._
import Types._

/**
  *
  * `Algebra`
  *
  * a function of type `F[A] => A` is an algebra of the functor `F`
  *
  *
  * `Fix`
  *
  * `Fix[F]` is the least fixed point of the functor `F`
  *
  * and
  *
  * `Fix: F[ Fix[F] ] => Fix[F]` is the initial algebra of `F`
  *
  *
  *                algebra
  * F[A] ------------------------> A
  * ^                              ^
  * |                              |
  * | _.map(_.cata(algebra)        | _.cata(algebra)
  * |                              |
  * |              Fix             |
  * F[ Fix[F] ] -----------------> Fix[F]
  *
  *
  * which naturally leads to the definition of `cata`
  *
  *
  *                algebra
  * F[A] ------------------------> A
  * ^                              ^
  * |                              |
  * | _.map(_.cata(algebra)        | _.cata(algebra)
  * |                              |
  * |              _.unFix         |
  * F[ Fix[F] ] <----------------- Fix[F]
  *
  *
  * Why is all this useful?
  *
  * `cata` generalizes structural recursion over common recursive types
  * for example, structural recursion over recursive lists (a.k.a. `foldRight`)
  * to a whole range of recursive data structures of type `Fix[F]`
  *
  * in order to define a structural recursive function of type `Fix[F] => A`
  * it suffices to define an algebra of type `F[A] => A`
  *
  * the nice thing about such structural recursive functions is that they can be defined in a *modular* way
  * because algebraic operations (constant, identity, sum and product) and recursion are strictly separated
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

trait FixTraverse[F[_, _]] extends Traverse[λ[Z => Fix[λ[ζ => F[Z, ζ]]]]] {
  def traverseAlgebra[A[_] : Applicative, Z, Y](z2ay: Z => A[Y]): Algebra[λ[ζ => F[Z, ζ]], A[Fix[λ[ζ => F[Y, ζ]]]]]

  def traverse[A[_] : Applicative, Z, Y](fix: Fix[λ[ζ => F[Z, ζ]]])(z2ay: Z => A[Y]): A[Fix[λ[ζ => F[Y, ζ]]]] =
    fix.cata(traverseAlgebra(z2ay))
}

