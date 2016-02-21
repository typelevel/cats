package cats
package arrow

import cats.functor.Strong

trait Arrow[F[_, _]] extends Split[F] with Strong[F] with Category[F] {
  self =>

  def lift[A, B](f: A => B): F[A, B]

  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] =
    compose(lift(g), andThen(lift(f), fab))

  def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = {
    def swap[X, Y]: F[(X, Y), (Y, X)] = lift[(X, Y), (Y, X)] { case (x, y) => (y, x) }
    compose(swap, compose(first[A, B, C](fa), swap))
  }

  def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] =
    andThen(first(f), second(g))

  // helper arrow: duplicates value
  def dupArrow[A]: F[A, (A, A)] = lift[A, (A, A)] { a => (a, a) }

  // helper arrow: applies function value to value
  def applyArrow[A, B]: F[(A, A => B), B] =
    lift {
      case (a, a2b) =>
        a2b(a)
    }

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
}

object Arrow {
  def apply[F[_, _]](implicit ev: Arrow[F]): Arrow[F] = ev
}
