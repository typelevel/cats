package cats
package data

import Or.{LeftOr, RightOr}
import algebra.{Order, Monoid}

/** Represents a right-biased disjunction that is either an `A` or a `B`.
 *
 * An instance of `A` [[Or]] `B` is either a [[LeftOr]]`[A]` or a [[RightOr]]`[B]`.
 *
 * A common use of [[Or]] is to explicitly represent the possibility of failure in a result as opposed to
 * throwing an exception.  By convention, [[LeftOr]] is used for errors and [[RightOr]] is reserved for successes.
 * For example, a function that attempts to parse an integer from a string may have a return type of
 * `NumberFormatException` [[Or]] `Int`. However, since there is no need to actually throw an exception, the type (`A`)
 * chosen for the "left" could be any type representing an error and has no need to actually extend `Exception`.
 *
 * `A` [[Or]] `B` is isomorphic to `scala.Either[A, B]`, but [[Or]] is right-biased, so methods such as `map` and
 * `flatMap` apply only in the context of the "right" case. This right bias makes [[Or]] more convenient to use
 * than `scala.Either` in a monadic context. Methods such as `swap`, and `leftMap` provide functionality
 * that `scala.Either` exposes through left projections.
 */
sealed abstract class Or[+A, +B] extends Product with Serializable {
  def fold[C](fa: A => C, fb: B => C): C = this match {
    case RightOr(b) => fb(b)
    case LeftOr(a) => fa(a)
  }

  def isLeft = fold(_ => true, _ => false)

  def isRight = fold(_ => false, _ => true)

  def swap: B Or A = fold(RightOr(_), LeftOr(_))

  def foreach(f: B => Unit): Unit = fold(_ => (), f(_))

  def getOrElse[BB >: B](default: => BB): BB = fold(_ => default, identity)

  def forall(f: B => Boolean) = fold(_ => true, f)

  def exists(f: B => Boolean) = fold(_ => false, f)

  def filter[AA >: A](f: B => Boolean)(implicit M: Monoid[AA]): AA Or B = fold(
    _ => this,
    b => if (f(b)) this else LeftOr(M.empty)
  )

  def toEither: Either[A, B] = fold(Left(_), Right(_))

  def toOption: Option[B] = fold(_ => None, Some(_))

  def toList: List[B] = fold(_ => Nil, _ :: Nil)

  def to[F[_], BB >: B](implicit monoidKF: MonoidK[F], applicativeF: Applicative[F]): F[BB] =
    fold(_ => monoidKF.empty, applicativeF.pure)

  def bimap[C, D](fa: A => C, fb: B => D): C Or D = this match {
    case RightOr(b) => RightOr(fb(b))
    case LeftOr(a) => LeftOr(fa(a))
  }

  def flatMap[AA >: A, D](f: B => AA Or D): AA Or D = this match {
    case RightOr(b) => f(b)
    case left @ LeftOr(_) => left
  }

  def map[D](f: B => D): A Or D = bimap(identity, f)

  def leftMap[C](f: A => C): C Or B = bimap(f, identity)

  def compare[AA >: A, BB >: B](that: AA Or BB)(implicit AA: Order[AA], BB: Order[BB]): Int = fold(
    a => that.fold(AA.compare(a, _), _ => -1),
    b => that.fold(_ => 1, BB.compare(b, _))
  )

  def traverse[F[_], AA >: A, D](f: B => F[D])(implicit F: Applicative[F]): F[AA Or D] = this match {
    case RightOr(b) => F.map(f(b))(Or.right _)
    case left @ LeftOr(_) => F.pure(left)
  }

  def foldLeft[C](c: C)(f: (C, B) => C): C = fold(_ => c, f(c, _))

  def foldRight[C](c: C)(f: (B, C) => C): C = fold(_ => c, f(_, c))

  def foldRight[C](c: Lazy[C])(f: (B, Lazy[C]) => C): Lazy[C] = fold(_ => c, b => Lazy(f(b, c)))

  def merge[AA >: A](implicit ev: B <:< AA): AA = fold(identity, ev.apply)
}

object Or extends OrFunctions {
  final case class LeftOr[+A](a: A) extends (A Or Nothing)

  final case class RightOr[+B](b: B) extends (Nothing Or B)

  implicit def orOrder[A: Order, B: Order]: Order[A Or B] = new Order[A Or B] {
    override def compare(x: A Or B, y: A Or B): Int = x compare y
  }

  implicit def orShow[A, B](implicit A: Show[A], B: Show[B]): Show[A Or B] = new Show[A Or B] {
    override def show(f: A Or B): String = f.fold(
      a => s"LeftOr(${A.show(a)})",
      b => s"RightOr(${B.show(b)})"
    )
  }

  implicit def orTraverse[A]: Traverse[A Or ?] = new Traverse[A Or ?] {
    override def traverse[F[_]: Applicative, B, C](fa: A Or B)(f: B => F[C]): F[A Or C] = fa.traverse(f)
    override def foldLeft[B, C](fa: A Or B, b: C)(f: (C, B) => C): C = fa.foldLeft(b)(f)
    override def foldRight[B, C](fa: A Or B, b: C)(f: (B, C) => C): C = fa.foldRight(b)(f)
    override def foldRight[B, C](fa: A Or B, b: Lazy[C])(f: (B, Lazy[C]) => C): Lazy[C] = fa.foldRight(b)(f)
  }

  implicit def orMonad[A]: Monad[A Or ?] = new Monad[A Or ?] {
    override def flatMap[B, C](fa: A Or B)(f: B => A Or C): A Or C = fa.flatMap(f)
    override def pure[B](b: B): A Or B = Or.right(b)

  }

}

trait OrFunctions {
  def left[A, B](a: A): A Or B = LeftOr(a)

  def right[A, B](b: B): A Or B = RightOr(b)
}

