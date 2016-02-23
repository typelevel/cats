package cats
package arrow

import cats.functor.Strong

trait Arrow[F[_, _]] extends Split[F] with Strong[F] with Category[F] {
  self =>

  //
  // standard arrow methods
  // see [[ https://www.haskell.org/arrows ]]
  //
  // defined methods are defined using `compose`,
  // they might as well be defined using `andThen`
  //

  def lift[A, B](f: A => B): F[A, B]

  def dimap[A, B, C, D](fab: F[A, B])(fca: C => A)(fbd: B => D): F[C, D] =
    compose(lift(fbd), compose(fab, lift(fca)))

  def second[A, B, C](fab: F[A, B]): F[(C, A), (C, B)] =
    compose(swapArrow, compose(first[A, B, C](fab), swapArrow))

  def split[A, B, C, D](fab: F[A, B], fcd: F[C, D]): F[(A, C), (B, D)] =
    compose(second(fcd), first(fab))

  //
  // arrow calculus related methods
  // see [[ http://homepages.inf.ed.ac.uk/wadler/papers/arrows/arrows.pdf ]]
  //
  // defined methods are defined using `andThen`,
  // they might as well be defined using `compose`
  //

  // uncurried version of flatDef
  def flatDefine[A, B, C](afb: F[A, B]): F[(A, B), C] => F[A, C] =
    abfc =>
      andThen(andThen(dupArrow, second[A, B, A](afb)), abfc)

  // curried version of flatDefine
  def flatDef[A, B, C](afb: F[A, B]): F[B, A => C] => F[A, C] =
    bfa2c =>
      andThen(andThen(dupArrow, second[A, A => C, A](andThen(afb, bfa2c))), applyArrow[A, C])

  // uncurried version of `def`
  def define[A, B, C](a2b: A => B): F[(A, B), C] => F[A, C] =
    flatDefine(lift(a2b))

  // curried version of define
  def `def`[A, B, C](a2b: A => B): F[B, A => C] => F[A, C] =
    flatDef(lift(a2b))

  // uncurried version of `val`
  def value[A, B, C](b: B): F[(A, B), C] => F[A, C] =
    define { _ => b }

  // curried version of value
  def `val`[A, B, C](b: B): F[B, A => C] => F[A, C] =
    `def` { _ => b }

  //
  // useful pure arrows
  // (obtained by lifting corresponding useful functions)
  //

  // swaps values
  def swapArrow[A, B]: F[(A, B), (B, A)] = lift[(A, B), (B, A)] { case (a, b) => (b, a) }

  // duplicates value
  def dupArrow[A]: F[A, (A, A)] = lift[A, (A, A)] { a => (a, a) }

  // applies function value to value
  def applyArrow[A, B]: F[(A, A => B), B] =
    lift { case (a, a2b) => a2b(a) }

}

object Arrow {
  def apply[F[_, _]](implicit ev: Arrow[F]): Arrow[F] = ev
}
