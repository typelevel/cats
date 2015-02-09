package cats
package data

import cats.data.Or.{LeftOr, RightOr}

import scala.reflect.ClassTag

/** Represents a right-biased disjunction that is either an `A` or a `B`.
 *
 * An instance of `A` [[Or]] `B` is either a [[Or.LeftOr LeftOr]]`[A]` or a [[Or.RightOr RightOr]]`[B]`.
 *
 * A common use of [[Or]] is to explicitly represent the possibility of failure in a result as opposed to
 * throwing an exception.  By convention, [[Or.LeftOr LeftOr]] is used for errors and [[Or.RightOr RightOr]] is reserved for successes.
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
    case LeftOr(a) => fa(a)
    case RightOr(b) => fb(b)
  }

  def isLeft: Boolean = fold(_ => true, _ => false)

  def isRight: Boolean = fold(_ => false, _ => true)

  def swap: B Or A = fold(RightOr(_), LeftOr(_))

  def foreach(f: B => Unit): Unit = fold(_ => (), f)

  def getOrElse[BB >: B](default: => BB): BB = fold(_ => default, identity)

  def forall(f: B => Boolean): Boolean = fold(_ => true, f)

  def exists(f: B => Boolean): Boolean = fold(_ => false, f)

  def filter[AA >: A](f: B => Boolean)(implicit M: Monoid[AA]): AA Or B = this match {
    case LeftOr(_) => this
    case RightOr(b) => if (f(b)) this else LeftOr(M.empty)
  }

  def toEither: Either[A, B] = fold(Left(_), Right(_))

  def toOption: Option[B] = fold(_ => None, Some(_))

  def toList: List[B] = fold(_ => Nil, _ :: Nil)

  def to[F[_], BB >: B](implicit monoidKF: MonoidK[F], applicativeF: Applicative[F]): F[BB] =
    fold(_ => monoidKF.empty, applicativeF.pure)

  def bimap[C, D](fa: A => C, fb: B => D): C Or D = this match {
    case LeftOr(a) => LeftOr(fa(a))
    case RightOr(b) => RightOr(fb(b))
  }

  def flatMap[AA >: A, D](f: B => AA Or D): AA Or D = this match {
    case left @ LeftOr(_) => left
    case RightOr(b) => f(b)
  }

  def map[D](f: B => D): A Or D = bimap(identity, f)

  def leftMap[C](f: A => C): C Or B = bimap(f, identity)

  def compare[AA >: A, BB >: B](that: AA Or BB)(implicit AA: Order[AA], BB: Order[BB]): Int = fold(
    a => that.fold(AA.compare(a, _), _ => -1),
    b => that.fold(_ => 1, BB.compare(b, _))
  )

  def partialCompare[AA >: A, BB >: B](that: AA Or BB)(implicit AA: PartialOrder[AA], BB: PartialOrder[BB]): Double = fold(
    a => that.fold(AA.partialCompare(a, _), _ => -1),
    b => that.fold(_ => 1, BB.partialCompare(b, _))
  )

  def ===[AA >: A, BB >: B](that: AA Or BB)(implicit AA: Eq[AA], BB: Eq[BB]): Boolean = fold(
    a => that.fold(AA.eqv(a, _), _ => false),
    b => that.fold(_ => false, BB.eqv(b, _))
  )

  def traverse[F[_], AA >: A, D](f: B => F[D])(implicit F: Applicative[F]): F[AA Or D] = this match {
    case left @ LeftOr(_) => F.pure(left)
    case RightOr(b) => F.map(f(b))(Or.right _)
  }

  def foldLeft[C](c: C)(f: (C, B) => C): C = fold(_ => c, f(c, _))

  def foldRight[C](c: C)(f: (B, C) => C): C = fold(_ => c, f(_, c))

  def foldLazy[C](c: Lazy[C])(f: B => Fold[C]): Lazy[C] =
    fold(_ => c, b => c.map(f(b).complete))

  def merge[AA >: A](implicit ev: B <:< AA): AA = fold(identity, ev.apply)

  def show[AA >: A, BB >: B](implicit AA: Show[AA], BB: Show[BB]): String = fold(
    a => s"LeftOr(${AA.show(a)})",
    b => s"RightOr(${BB.show(b)})"
  )

}

object Or extends OrInstances with OrFunctions {
  final case class LeftOr[+A](a: A) extends (A Or Nothing)

  final case class RightOr[+B](b: B) extends (Nothing Or B)
}

sealed abstract class OrInstances extends OrInstances1 {
  implicit def orOrder[A: Order, B: Order]: Order[A Or B] = new Order[A Or B] {
    def compare(x: A Or B, y: A Or B): Int = x compare y
    override def partialCompare(x: A Or B, y: A Or B): Double = x partialCompare y
    override def eqv(x: A Or B, y: A Or B): Boolean = x === y
  }

  implicit def orShow[A, B](implicit A: Show[A], B: Show[B]): Show[A Or B] = new Show[A Or B] {
    def show(f: A Or B): String = f.show
  }

  implicit def orInstances[A] = new OrInstances[A]
  class OrInstances[A] extends Traverse[A Or ?] with Monad[A Or ?] {
    def traverse[F[_]: Applicative, B, C](fa: A Or B)(f: B => F[C]): F[A Or C] = fa.traverse(f)
    def foldLeft[B, C](fa: A Or B, b: C)(f: (C, B) => C): C = fa.foldLeft(b)(f)
    def foldRight[B, C](fa: A Or B, b: C)(f: (B, C) => C): C = fa.foldRight(b)(f)
    def foldLazy[B, C](fa: A Or B, b: Lazy[C])(f: B => Fold[C]): Lazy[C] = fa.foldLazy(b)(f)

    def flatMap[B, C](fa: A Or B)(f: B => A Or C): A Or C = fa.flatMap(f)
    def pure[B](b: B): A Or B = Or.right(b)

    override def map[B, C](fa: A Or B)(f: B => C): A Or C = fa.map(f)
  }
}

sealed abstract class OrInstances1 extends OrInstances2 {
  implicit def orPartialOrder[A: PartialOrder, B: PartialOrder]: PartialOrder[A Or B] = new PartialOrder[A Or B] {
    def partialCompare(x: A Or B, y: A Or B): Double = x partialCompare y
    override def eqv(x: A Or B, y: Or[A, B]): Boolean = x === y
  }
}

sealed abstract class OrInstances2 {
  implicit def orEq[A: Eq, B: Eq]: Eq[A Or B] = new Eq[A Or B] {
    def eqv(x: A Or B, y: Or[A, B]): Boolean = x === y
  }
}

trait OrFunctions {
  def left[A, B](a: A): A Or B = LeftOr(a)

  def right[A, B](b: B): A Or B = RightOr(b)

  /**
   * Catch a specified `Throwable` ('`T`') instance and return it wrapped in an `Or[T, A]`,
   * where `A` is the valid return value (inferred from function block)
   */
  def fromTryCatch[T >: Null <: Throwable]: FromTryCatchAux[T] = new FromTryCatchAux[T]

  class FromTryCatchAux[T] private[OrFunctions] {
    def apply[A](f: => A)(implicit T: ClassTag[T]): T Or A = {
      try {
        Or.RightOr(f)
      } catch {
        case t if T.runtimeClass.isInstance(t) =>
          Or.LeftOr(t.asInstanceOf[T])
      }
    }
  }
}

