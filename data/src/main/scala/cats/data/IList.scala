package cats.data

import algebra.Monoid
import cats.Fold.{Continue, Return}
import cats._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Purely functional single linked list
 * IList stands for Invariant List as opposed to [[scala.List]] which is covariant
 */
sealed abstract class IList[A] extends Product with Serializable {
  import cats.data.IList._

  /** add an element to the back */
  final def append(a: A): IList[A] =
    reverse.foldLeft(IList.singleton(a))((acc, a) => ICons(a, acc))

  /** add an [[IList]] to the back */
  final def concat(as: IList[A]): IList[A] =
    reverse.foldLeft(as)((acc, a) => ICons(a, acc))

  /** alias for concat */
  final def ++(as: IList[A]): IList[A] =
    reverse.foldLeft(as)((acc, a) => ICons(a, acc))

  /** drop the `n` first elements */
  @tailrec final def drop(n: Int): IList[A] = this match {
    case INil()     => this
    case ICons(h,t) => if(n > 0) t.drop(n - 1) else this
  }

  /** drop elements as long as the predicate holds */
  @tailrec final def dropWhile(p: A => Boolean): IList[A] = this match {
    case INil() => this
    case ICons(h,t) => if(p(h)) t.dropWhile(p) else this
  }

  /** filter all elements that match the predicate */
  final def filter(p: A => Boolean): IList[A] = {
    @tailrec
    def loop(as: IList[A], acc: IList[A]): IList[A] = as match {
      case INil()      => acc.reverse
      case ICons(h, t) => if(p(h)) loop(t, ICons(h, acc)) else loop(t, acc)
    }
    loop(this, empty[A])
  }

  final def flatMap[B](f: A => IList[B]): IList[B] =
    reverse.foldLeft(empty[B])((acc, a) => f(a) ++ acc )

  final def foldLazy[B](zero: Lazy[B])(f: A => Fold[B]): Lazy[B] = {
    @tailrec
    def loop(as: IList[A], fs: IList[B => B]): B = as match {
      case INil()      => fs.foldLeft(zero.force)((acc, f) => f(acc))
      case ICons(h, t) => f(h) match {
        case Return(b)   => fs.foldLeft(b)((acc, f) => f(acc))
        case Continue(g) => loop(t, g :: fs)
        case pass        => loop(t, fs)
      }
    }
    Lazy(loop(this, empty[B => B]))
  }

  @tailrec
  final def foldLeft[B](b: B)(f: (B, A) => B): B = this match {
    case INil()      => b
    case ICons(h, t) => t.foldLeft(f(b,h))(f)
  }

  final def foldMap[B](b: B)(f: A => B)(implicit B: Monoid[B]): B =
    reverse.foldLeft(b)((acc, a) => B.combine(f(a), acc))

  final def foldRight[B](b: B)(f: (A, B) => B): B =
    reverse.foldLeft(b)((b, a) => f(a, b))

  /** get the head if the [[IList]] is not empty */
  final def headOption: Option[A] = this match {
    case INil()      => Option.empty
    case ICons(h, _) => Some(h)
  }

  /** check if an [[IList]] is empty */
  final def isEmpty: Boolean = this match {
    case INil()      => true
    case ICons(_, _) => false
  }

  /** get the last element if the [[IList]] is not empty */
  final def lastOption: Option[A] = {
    @tailrec def loop(as: IList[A]): A = (as: @unchecked) match {
      case ICons(h, INil()) => h
      case ICons(_, t)      => loop(t)
    }
    if(isEmpty) None
    else Some(loop(this))
  }

  /** get the element at the index if it exists */
  final def lookup(index: Int): Option[A] = {
    @tailrec
    def loop(as: IList[A], i: Int): Option[A] = as match {
      case ICons(h, t) =>
        if(i > 0) loop(t, i - 1)
        else if(i == 0) Some(h)
        else None
      case INil() => None
    }
    loop(this, index)
  }

  final def map[B](f: A => B): IList[B] =
    reverse.foldLeft(empty[B])((acc, a) => ICons(f(a), acc))

  /** add an element to the front */
  final def prepend(a: A): IList[A] =
    ICons(a, this)

  /** alias for prepend */
  final def ::(a: A): IList[A] =
    ICons(a, this)

  /** reverse an [[IList]] */
  final def reverse: IList[A] =
    foldLeft(empty[A])((acc, a) => ICons(a, acc))

  /** provide a textual representation of an [[IList]] */
  final def show(implicit A: Show[A]): String =
    "[" + toNel.fold("")(_.reduceLeft(A.show)(_ + "," + A.show(_))) + "]"

  /** compute the size of an [[IList]] */
  final def size: Int = {
    @inline @tailrec def loop(as: IList[A], acc: Int): Int = as match {
      case ICons(_, t) => loop(t, acc + 1)
      case INil()      => acc
    }
    loop(this, 0)
  }

  /** get the tail if the [[IList]] is not empty */
  final def tailOption: Option[IList[A]] = this match {
    case INil()      => Option.empty
    case ICons(_, t) => Some(t)
  }

  /** take the `n` first elements */
  final def take(n: Int): IList[A] = {
    @tailrec
    def loop(as: IList[A], m: Int, acc: IList[A]): IList[A] = as match {
      case INil() => acc.reverse
      case ICons(h,t) => if(m > 0) loop(t, m - 1, ICons(h, acc)) else acc.reverse
    }
    loop(this, n, empty)
  }

  /** take elements as long as the predicate holds */
  final def takeWhile(p: A => Boolean): IList[A] = {
    @tailrec
    def loop(as: IList[A], acc: IList[A]): IList[A] = as match {
      case INil() => acc.reverse
      case ICons(h,t) => if(p(h)) loop(t, ICons(h, acc)) else acc.reverse
    }
    loop(this, empty)
  }

  /** transform an [[IList]] into a [[scala.List]] */
  final def toList: List[A] =
    foldLeft(ListBuffer.empty[A])(_ += _).toList

  /** attempt to transform an [[IList]] into a [[NonEmptyList]] */
  final def toNel: Option[NonEmptyList[A]] =
    uncons.map{ case (h,t) => NonEmptyList(h, t) }

  /**
   * `toString` will give the same result as `show` if there exists a `Show[A]`
   * instance and `Show[A].show` is equal to `toString`
   */
  override final def toString: String =
    "[" + toNel.fold("")(_.reduceLeft(_.toString)((acc, a) => a.toString + "," + acc)) + "]"

  final def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[IList[B]] =
    reverse.foldLeft(G.pure(empty[B]))((acc, a) => G.map2(acc, f(a))(_.prepend(_)))

  /** attempt to get head and tail of an [[IList]] */
  final def uncons: Option[(A, IList[A])] = this match {
    case INil()      => Option.empty
    case ICons(h, t) => Some((h,t))
  }

  /** widen the type of an [[IList]] */
  final def widen[B >: A]: IList[B] =
    this.asInstanceOf[IList[B]]

  /** check if two matches are equal */
  final def ===(other: IList[A])(implicit A: Eq[A]): Boolean = {
    @inline @tailrec
    def loop(as: IList[A], bs: IList[A]): Boolean = (as, bs) match {
      case (INil(), INil()) => true
      case (ICons(x, xs), ICons(y, ys)) => A.eqv(x,y) && loop(xs, ys)
      case _ => false
    }
    loop(this, other)
  }

}

object IList extends IListInstances {
  final case class INil[A]() extends IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]

  private val nil: IList[Nothing] = INil()

  /** create an [[IList]] with a single element */
  def singleton[A](a: A): IList[A] =
    ICons(a, empty)

  /** create an empty [[IList]] */
  def empty[A]: IList[A] =
    nil.asInstanceOf[IList[A]]

  /** create an [[IList]] from a varargs */
  def apply[A](as: A*): IList[A] =
    as.foldRight(empty[A])(ICons(_,_))

  /** create an [[IList]] from a [[Foldable]] */
  def fromFoldable[F[_], A](fa: F[A])(implicit F: Foldable[F]): IList[A] =
    F.foldRight(fa, empty[A])(ICons(_,_))
}

sealed abstract class IListInstances {
  implicit def ilistEq[A: Eq]: Eq[IList[A]] = new Eq[IList[A]]{
    def eqv(x: IList[A], y: IList[A]): Boolean =
      x === y
  }

  implicit def ilistShow[A: Show]: Show[IList[A]] = new Show[IList[A]] {
    def show(f: IList[A]): String =
      f.show
  }

  implicit def ilistTraverse: Traverse[IList] = new Traverse[IList] {
    def traverse[G[_]: Applicative, A, B](fa: IList[A])(f: A => G[B]): G[IList[B]] =
      fa.traverse(f)

    def foldLeft[A, B](fa: IList[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    def foldLazy[A, B](fa: IList[A], b: Lazy[B])(f: A => Fold[B]): Lazy[B] =
      fa.foldLazy(b)(f)
  }

  implicit def iListMonadCombine: MonadCombine[IList] = new MonadCombine[IList] {
    def pure[A](x: A): IList[A] =
      IList.singleton(x)

    def flatMap[A, B](fa: IList[A])(f: A => IList[B]): IList[B] =
      fa.flatMap(f)

    override def map[A, B](fa: IList[A])(f: A => B): IList[B] =
      fa.map(f)

    override def filter[A](fa: IList[A])(f: A => Boolean): IList[A] =
      fa.filter(f)

    def empty[A]: IList[A] =
      IList.empty

    def combine[A](x: IList[A], y: IList[A]): IList[A] =
      x concat y
  }

  implicit def ilistMonoid[A]: Monoid[IList[A]] = new Monoid[IList[A]] {
    def empty: IList[A] =
      IList.empty

    def combine(x: IList[A], y: IList[A]): IList[A] =
      x concat y
  }
}
